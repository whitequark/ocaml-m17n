module Typecore = struct
  open Format
  open Printtyp

  let report_error env ppf = function
    | Typecore.Expr_type_clash trace ->
        report_unification_error ppf env trace
          (function ppf ->
             fprintf ppf "此表达式具有类型")
          (function ppf ->
             fprintf ppf "，但预期类型为*的表达式")
    | _ -> raise Exit

  let report_error env ppf err =
    wrap_printing_env env (fun () -> report_error env ppf err)

  let () =
    Location.register_error_of_exn
      (function
        | Typecore.Error (loc, env, err) ->
          begin try
            Some (Location.error_of_printer loc (report_error env) err)
          with Exit ->
            None
          end
        | _ ->
          None
      )
end
