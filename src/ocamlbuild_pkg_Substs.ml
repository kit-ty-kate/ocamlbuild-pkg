open Ocamlbuild_plugin

let dispatcher files substs = function
  | After_rules ->
      let aux file =
        rule (file ^ ".in -> " ^ file)
          ~dep:(file ^ ".in")
          ~prod:file
          (fun env _ ->
             let dep = env (file ^ ".in") in
             let prod = env file in
             let content = read_file dep in
             let content =
               List.fold_left
                 (fun content (pat, repl) -> String.subst pat repl content)
                 content
                 substs
             in
             Echo ([content], prod)
          )
      in
      List.iter aux files;
  | _ ->
      ()
