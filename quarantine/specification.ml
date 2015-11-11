open Core_kernel.Std
open Bap.Std
open ARM.CPU
open Spec.Language

(** Introduces taint, but no model?
  define "recvfrom_reaches_system" [
    rule "if_system_argument_depends" (* rule name *)
      [p := term v; (* any term *) (* rule premise *)
       r := sub "recvfrom" []] (* rule premise *)
      [z := sub "system" [y]] (* rule conclusion *)
  ] [r/p; y/p]; (* constraints. Input to system depends on buffer of recv (arg1) *)
*)

let spec = [
  define "recvfrom_reaches_system" [
    rule "if_system_argument_depends" (* rule name *)
      [
        p := term v;
        call "system" [x]
      ]
      [call "recvfrom" [y]]
  ] [y=r1; x=r0; y/p; ]; (* can't add x/p :'( *)
  (** Setting an argument constraint x=r0|r1 of a premise causes no
      models to generate*)

  define "malloc_is_safe" [
    rule "if_some_jmp_depends"
      [p := sub "malloc" []]
      [case c jmp x]
  ] [c/p; p = r0];

  define "calloc_is_safe" [
    rule "if_some_jmp_depends"
      [p := sub "calloc" []]
      [case c jmp x]
  ] [c/p; p = r0];

  (* define "malloc_is_safe_and_used" [ *)
  (*   rule "if_used_and_some_jmp_depends" *)
  (*     [p := sub "malloc" []; use t] *)
  (*     [case c jmp x] *)
  (* ] [c/p; t/p; p = r0]; *)

  (* define "magic_door_exists" [ *)
  (*   rule "when_magic_meets_user_input" [ *)
  (*     p := term v; *)
  (*     x := sub "read" [] *)
  (*   ][ *)
  (*     case c jmp d *)
  (*   ] *)
  (* ][ *)
  (*   such v that is_black; *)
  (*   x = r0; *)
  (*   c / x; *)
  (*   c / p; *)
  (* ]; *)

  (* define "sql_exec_is_safe" [ *)
  (*   rule "if_escaped_before_exec" [ *)
  (*     x := term u; *)
  (*     call "sql_exec"[p] *)
  (*   ][ *)
  (*     z := sub "sql_escape"[y] *)
  (*   ] *)
  (* ] [p = r0; p/x; y/x; p/z; such u that is_black] *)
]
