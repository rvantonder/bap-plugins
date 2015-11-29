open Strtol_check
open Memcpy_check
open System_check
open System_simple_check
open Snprintf_check
open Sprintf_check
open Strcpy_check
open Sql_check

let strtol_check = Strtol_check.check
let memcpy_check = Memcpy_check.check
let system_check = System_check.check
let system_simple_check = System_simple_check.check
let snprintf_check = Snprintf_check.check
let sprintf_check = Sprintf_check.check
let strcpy_check = Strcpy_check.check
let sql_check = Sql_check.check
let ident : Check.t = Check.(
    {should_produce = (fun _ -> true);
     run = (fun _ -> 5);
     reverse=false;
     max_depth=(-1);
     sample=(-1);
     timeout=(-1)})

let select = function
    | "memcpy" -> memcpy_check
    | "sql" -> sql_check
    | "system" -> system_check
    | "system_simple" -> system_simple_check
    | "strcpy" -> strcpy_check
    | "atoi" -> strtol_check
    | "snprintf" -> snprintf_check
    | "sprintf" -> sprintf_check
    | _ -> ident
