open Core_kernel.Std
open Bap.Std
open Check
open Cabs
open Util

module SM = Monad.State
open SM.Monad_infix

type vars = Tid.Set.t Var.Map.t Tid.Map.t

let target_of_goto jmp = match Jmp.kind jmp with
  | Goto (Direct tid) -> Some tid
  | _ -> None

let def_const = Word.zero 8

(** I don't need summaries, remove these later *)
let summaries = String.Map.of_alist_exn []

let def_summary call = match Call.target call with
  | Indirect _ -> None
  | Direct tid ->
    Map.find summaries (Tid.name tid)


class context p total  = object(self : 's)
  inherit Taint.context as taints
  inherit Biri.context p as super

  val blk : blk term option = None
  val k = total
  val vis : Tid.Set.t = Tid.Set.empty (* visited *)
  val cps : 's Tid.Map.t = Tid.Map.empty (* checkpoints *)
  val tvs : vars = Tid.Map.empty

  method k   = k
  method tvs = tvs
  method cps = cps
  method visited = vis

  method enter_blk blk = {< blk = Some blk >}

  method blk = blk

  method step =
    if k > 0
    then Some {< k = k - 1 >}
    else None

  method visit_term tid =
    {< vis = Set.add vis tid; cps = Map.remove cps tid; >}

  method checkpoint tid =
    if Set.mem vis tid then self
    else {< cps = Map.add cps ~key:tid ~data:self>}

  method backtrack : (tid * 's) option =
    match Map.min_elt cps with
    | None -> None
    | Some (tid,other) ->
      Some (tid,other#merge self tid)

  method merge runner tid =
    {<
      k   = runner#k;
      vis = runner#visited;
      tvs = runner#tvs;
      cps = runner#cps
    >}

  method taint_var tid v r =
    let ts = taints#val_taints r in
    let tvs = Map.change tvs tid (function
        | None when Set.is_empty ts -> None
        | None -> Some (Var.Map.of_alist_exn [v, ts])
        | Some vs -> Option.some @@ Map.change vs v (function
            | None when Set.is_empty ts -> None
            | None -> Some ts
            | Some ts' -> Some (Set.union ts ts'))) in
    {< tvs = tvs >}

  method taints_of_term tid =
    Map.find tvs tid |> function
    | None -> Var.Map.empty
    | Some ts -> ts
end

class ['a] main summary memory tid_of_addr const = object(self)
  constraint 'a = #context
  inherit ['a] biri as super
  inherit ['a] Taint.propagator

  method! enter_term cls t =
    let tid = Term.tid t in
    SM.get () >>= fun c ->
    SM.put (c#visit_term tid) >>= fun () ->
    super#enter_term cls t

  method! eval_unknown _ t = self#emit t

  method! lookup v =
    super#lookup v >>= fun r ->
    SM.get () >>= fun ctxt ->
    match List.hd ctxt#trace with
    | None -> SM.return r
    | Some tid ->
      SM.put (ctxt#taint_var tid v r) >>= fun () ->
      match Bil.Result.value r with
      | Bil.Imm _ | Bil.Mem _ -> SM.return r
      | Bil.Bot -> self#emit (Var.typ v)

  method! load s a =
    super#load s a >>= fun r -> match Bil.Result.value r with
    | Bil.Imm _ | Bil.Mem _ -> SM.return r
    | Bil.Bot -> match memory a with
      | None -> self#emit_const 8
      | Some w ->
        SM.get () >>= fun ctxt ->
        let ctxt,r = ctxt#create_word w in
        SM.put ctxt >>= fun () ->
        SM.return r


  method! eval_blk blk =
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#enter_blk blk) >>= fun () ->
    super#eval_blk blk

  method! eval_jmp jmp =
    SM.get () >>= fun ctxt ->
    match ctxt#step with
    | None -> SM.put (ctxt#set_next None)
    | Some ctxt ->
      SM.put ctxt >>= fun () ->
      super#eval_jmp jmp
      >>= fun () ->
      match ctxt#blk with
      | None -> assert false
      | Some blk ->
        Term.enum jmp_t blk |>
        Seq.fold ~init:(SM.return ()) ~f:(fun m jmp ->
            m >>= fun () ->
            match target_of_goto jmp with
            | None -> SM.return ()
            | Some tid ->
              SM.get () >>= fun ctxt ->
              SM.put (ctxt#checkpoint tid))

  method! eval_call call =
    self#shortcut_indirect call >>= fun () ->
    self#summarize_call call

  method! eval_indirect exp =
    self#eval_exp exp >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> self#backtrack
    | Bil.Imm dst ->
      match tid_of_addr dst with
      | Some dst -> self#eval_direct dst
      | None -> self#backtrack

  method private backtrack  =
    SM.get () >>= fun ctxt ->
    match ctxt#backtrack with
    | None -> SM.put (ctxt#set_next None)
    | Some (next,ctxt) ->
      SM.put ctxt >>= fun () -> self#eval_direct next

  method! eval_def def =
    match Term.get_attr def Taint.seed with
    | None -> super#eval_def def
    | Some seed ->
      super#eval_def def >>= fun () ->
      SM.get () >>= fun ctxt ->
      super#lookup (Def.lhs def) >>= fun x ->
      SM.put (ctxt#taint_val x (Tid.Set.singleton seed)) >>= fun () ->
      self#update (Def.lhs def) x

  method private emit t =
    match t with
    | Type.Imm sz -> self#emit_const sz
    | Type.Mem _  -> self#emit_empty

  method private emit_const sz =
    SM.get () >>= fun ctxt ->
    let const = Word.extract_exn ~lo:0 ~hi:(sz-1) const in
    let ctxt,r = ctxt#create_word const in
    SM.put ctxt >>= fun () ->
    SM.return r

  method private emit_empty =
    SM.get () >>= fun ctxt ->
    let ctxt,r = ctxt#create_storage self#empty in
    SM.put ctxt >>= fun () ->
    SM.return r

  method private shortcut_indirect call =
    match Call.target call with
    | Direct _ -> self#call_with_restore call
    | Indirect _ -> self#return call

  method private call_with_restore call =
    match Call.return call with
    | None | Some (Indirect _) -> super#eval_call call
    | Some (Direct ret) ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#checkpoint ret) >>= fun () ->
      super#eval_call call

  method private summarize_call call =
    let create f =
      SM.get () >>= fun ctxt ->
      let ctxt, v = f ctxt in
      SM.put ctxt >>= fun () ->
      SM.return v in
    match summary call with
    | None -> super#eval_call call
    | Some summary ->
      List.fold summary ~init:(SM.return ()) ~f:(fun m (x,v) ->
          m >>= fun () -> create (fun ctxt -> match v with
              | Bil.Mem s -> ctxt#create_storage s
              | Bil.Imm w -> ctxt#create_word w
              | Bil.Bot   -> ctxt#create_undefined) >>= fun r ->
          self#update x r) >>= fun () ->
      self#return call

  method private return call = match Call.return call with
    | None -> super#eval_call call
    | Some lab -> super#eval_ret lab
end

let create_mapping prog =
  let addrs = Addr.Table.create () in
  let add t a = Hashtbl.replace addrs ~key:a ~data:(Term.tid t) in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      Term.enum blk_t sub |> Seq.iter  ~f:(fun blk ->
          match Term.get_attr blk Disasm.block with
          | Some addr -> add blk addr
          | None -> ());
      match Term.get_attr sub subroutine_addr with
      | Some addr -> add sub addr
      | None -> ());
  Hashtbl.find addrs

let memory_lookup proj addr =
  let memory = Project.memory proj in
  Memmap.lookup memory addr |> Seq.hd |> function
  | None -> None
  | Some (mem,_) -> match Memory.get ~addr mem with
    | Ok w -> Some w
    | _ -> None

exception Entry_point_not_found

let tid_of_name str =
  match Tid.from_string ("@"^str) with
  | Ok tid -> tid
  | Error _ -> raise Entry_point_not_found

let tid_of_ident mapping = function
  | `Term tid -> tid
  | `Name str -> tid_of_name str
  | `Addr add -> match mapping add with
    | None -> Format.printf "In tid_of_ident\n%!"; raise Entry_point_not_found
    | Some tid -> tid

let run_from_tid p (biri : 'a #main) tid =
  match Program.lookup sub_t p tid with
  | Some sub -> biri#eval_sub sub
  | None -> Format.printf "In run_from_tid\n%!"; raise Entry_point_not_found

let run_from_point mapping p biri point =
  run_from_tid p biri (tid_of_ident mapping point)

class type result = object
  method visited : Tid.Set.t
  method taints_of_term : tid -> Taint.taints Var.Map.t
end

(** ABI stuff *)

type arg_size =
  | Word                        (** same size as CPU word  *)
  | Size of Size.t              (** the specified size     *)

type pos =
  | Ret_0
  | Ret_1
  | Arg of int

type arg = {
  arg_name : string;
  arg_pos  : pos;
  arg_intent : intent option;
  arg_size : arg_size;
}

let ret_word n = {
  arg_pos = n;
  arg_name = "result";
  arg_intent = Some Out;
  arg_size = Word; (* compiler will cast return value itself *)
}

include struct
  let stack mem sp endian sz off =
    let width = Size.to_bits sz in
    let off = Word.of_int ~width (off * (Size.to_bytes sz)) in
    let mem = Bil.var mem in
    let addr = if Word.is_zero off
      then Bil.(var sp)
      else Bil.(var sp + int off) in
    Bil.load ~mem ~addr endian sz

  let arm_stack = ARM.CPU.(stack mem sp LittleEndian `r32)
  let x86_stack = IA32.CPU.(stack mem sp LittleEndian `r32)
  let x64_stack = AMD64.CPU.(stack mem sp LittleEndian `r64)

  open Bil
  let abi = function
    | #Arch.arm ->
      ARM.CPU.(function
          | Ret_0 -> var r0
          | Ret_1 -> var r1
          | Arg 0 -> var r0
          | Arg 1 -> var r1
          | Arg 2 -> var r2
          | Arg 3 -> var r3
          | Arg n -> arm_stack Int.(n-4))
    | `x86_64 ->
      AMD64.CPU.(function
          | Ret_0 -> var rax
          | Ret_1 -> var rdx
          | Arg 0 -> var rdi
          | Arg 1 -> var rsi
          | Arg 2 -> var rdx
          | Arg 3 -> var rcx
          | Arg 4 -> var r.(0)
          | Arg 5 -> var r.(1)
          | Arg n -> x64_stack Int.(n-6))
    | `x86 ->
      IA32.CPU.(function
          | Ret_0 -> var rax
          | Ret_1 -> var rdx
          | Arg n -> x86_stack Int.(n+1))
    | _ -> raise Not_found
end

let (>:) s1 s2 e =
  match Size.(to_bits s2 - to_bits s1) with
  | 0 -> e
  | n when n > 0 -> Bil.(cast high n e)
  | n -> Bil.(cast low n e)

let term_of_arg arch sub {arg_name; arg_size; arg_intent; arg_pos} =
  let word_size = Arch.addr_size arch in
  let size = match arg_size with
    | Word -> (word_size :> Size.t)
    | Size size -> size in
  let typ = Type.imm (Size.to_bits size) in
  let exp = try abi arch arg_pos with
      exn -> Bil.unknown "unkown abi" typ in
  (* so far we assume, that [abi] returns expressions of word size,
     if this will ever change, then we need to extend abi function
     to return the size for us. *)
  let exp = (word_size >: size) exp in
  let var = Var.create (Sub.name sub ^ "_" ^ arg_name) typ in
  Arg.create ?intent:arg_intent var exp

let fill_args arch fns program =
  Term.map sub_t program ~f:(fun sub ->
      match Map.find fns (Sub.name sub) with
      | None -> sub
      | Some args ->
        List.fold args ~init:sub ~f:(fun sub arg ->
            Term.append arg_t sub (term_of_arg arch sub arg)))

let intent_of_type = function
  | PTR (CONST _) -> Some In
  | PTR (_) -> Some Both
  | _ -> Some In

let size_of_type typ = match typ with
  | PTR _ -> Word
  | CHAR _ -> Size `r8
  | INT (SHORT,_) -> Size `r16
  | INT (LONG_LONG,_) -> Size `r64
  | FLOAT _ -> Size `r32
  | DOUBLE false -> Size `r64
  | DOUBLE true -> Size `r128
  | _ -> Word

let arg_of_name n = function
  | (_,VOID,_,_) -> None
  | (name,typ,_,_) -> Some {
      arg_pos = Arg n;
      arg_name = if name <> "" then name else sprintf "x%d" (n+1);
      arg_intent = intent_of_type typ;
      arg_size = size_of_type typ;
    }

let string_of_single_name n (_,_,name) = arg_of_name n name

let args_of_single_names = List.filter_mapi ~f:string_of_single_name

let push = List.map ~f:(fun a -> match a with
    | {arg_pos = Arg n} -> {a with arg_pos = Arg (n+1)}
    | a -> a)

let fn_of_definition = function
  | DECDEF (_,_,[(name, PROTO (ret,args,false),[],NOTHING)])
  | FUNDEF ((_,_,(name, PROTO (ret,args,false),[], NOTHING)), _) ->
    let args = args_of_single_names args in
    let args = match ret with
      | VOID -> args
      | STRUCT _ | CONST (STRUCT _) ->
        {(ret_word (Arg 0)) with arg_intent = Some In} :: push args
      | INT (LONG_LONG,_) -> ret_word Ret_0 :: ret_word Ret_1 :: args
      | _ -> ret_word Ret_0 :: args in
    Some (name,args)
  | _ -> None

let fns_of_definitions =
  List.fold ~init:String.Map.empty ~f:(fun fns defn ->
      match fn_of_definition defn with
      | None -> fns
      | Some (name,data) -> Map.add fns ~key:name ~data)

let args_of_file file =
  let open Frontc in
  match Frontc.parse_file file stderr with
  | PARSING_ERROR -> raise Parsing.Parse_error
  | PARSING_OK ds -> fns_of_definitions ds

(** /ABI stuff *)

let display_tainted ctxt tid =
  let taint_vars = ctxt#taints_of_term tid in
  Format.printf "Displaying taint for sink tid %a\n%!" Tid.pp tid;
  Format.printf "\t Map size: %d\n%!" @@ Map.length taint_vars;
  if Map.length taint_vars > 0 then
    begin
      Map.iter taint_vars ~f:(fun ~key ~data ->
          Format.printf "tid %a : key %a taints\n%a%!"
            Tid.pp tid
            Var.pp key
            Taint.pp_taints data);
      Format.printf "\n%!"
    end
  else ()

let seed t =
  Term.set_attr t Taint.seed (Term.tid t)


let consume sub_path (check : Check.t) (ctxt : Check.ctxt) =
  (*
  (** Wrap sub_path in a program term so it doesn't complain about
      entry point not found. It still wants a sub within program though *)
  let prog =
    let new_p = Program.Builder.create () in
    Program.Builder.add_sub new_p sub_path;
    (** Also add sprintf/system calls *)
    let sprintf_tid = Tid.(!"@sprintf") in
    let system_tid = Tid.(!"@system") in
    let sub1 = Util.sub_of_tid ctxt.project sprintf_tid |> Util.val_exn in
    let sub2 = Util.sub_of_tid ctxt.project system_tid |> Util.val_exn in
    Program.Builder.add_sub new_p sub1;
    Program.Builder.add_sub new_p sub2;
    Program.Builder.result new_p in

  let arch = Project.arch ctxt.project in
  let args = args_of_file "libc.h" in
  let prog = fill_args arch args prog in

  Format.printf "Path (as program, with args): \n \
                 %a\n====================\n" Program.pp prog;

  let prog = Term.map sub_t prog ~f:(fun sub ->
      Term.map blk_t sub ~f:(fun blk ->
          Term.map def_t blk ~f:(fun def ->
              Format.printf "Seeding %a:%a" Tid.pp (Term.tid def) Def.pp def;
              seed def))) in
  (** Run taint over this path *)
  let start_sub = Term.first sub_t prog |> Util.val_exn |> Term.tid in
  (**                  Term.first blk_t |> Util.val_exn |>
                       Term.first def_t |> Util.val_exn |> Term.tid in*)

  Format.printf "Start point: %a\n%!" Tid.pp start_sub;
  let k = 500 in (** needed? *)
  (*let p = Project.program ctxt.project in*) (* Nope, you will use my program term *)
  (** This is the unified context over taint and biri *)
  let taint_ctxt = new context prog k in
  (** This is a mapping of sub to blk addrs *)
  let mapping = create_mapping prog in
  (** Fetch the value at a given address *)
  let memory = memory_lookup ctxt.project in
  (** The interpreter, instantiated with context *)
  let biri = new main def_summary memory mapping def_const in
  let map _ = None in (** no idea *)
  let res = run_from_point map prog biri (`Term start_sub) in
  let exec_res = (SM.exec res taint_ctxt :> result) in
  (** Check taint of first def in the last (sink) blk *)
  let first_sub = Term.first sub_t prog |> Util.val_exn in
  (*let tid = Util.blk_of_tid first_sub ctxt.trim.sink_tid |> Term.first def_t |>
            Util.val_exn |> Term.tid in*)
  let all_tids = Term.enum sub_t prog |> Seq.fold ~init:Seq.empty ~f:(fun acc sub ->
      Term.enum blk_t sub |> Seq.fold ~init:acc ~f:(fun acc blk ->
          Term.enum def_t blk |> Seq.fold ~init:acc ~f:(fun acc def ->
              (Term.tid def) ^:: acc))) in
  Seq.iter all_tids ~f:(fun tid ->
      display_tainted exec_res tid);
*)

  (** Priority type may change, so stick with pattern matching *)
  (** Without SSA, dependence matching fails and so does my check. comment out for now*)
  try
    match Util.timeout ~secs:1 ~f:check.run ~x:ctxt with
    | p -> Output.path_priority ctxt.path_dir ctxt.count p
  with
  | Timeout -> Format.printf "TIMEOUT!\n%!"; ()
