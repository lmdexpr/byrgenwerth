module type Schema = sig
  val field : (Dream.request, unit) Graphql_lwt.Schema.field
end

module User : Schema = struct
  open Graphql_lwt.Schema

  type t =
    { id : int
    ; name : string
    }

  let hardcoded = [ { id = 1; name = "alice" }; { id = 2; name = "bob" } ]

  let id =
    let typ = non_null int and args = Arg.[] and resolve _ u = u.id in
    field "id" ~typ ~args ~resolve

  let name =
    let typ = non_null string and args = Arg.[] and resolve _ u = u.name in
    field "name" ~typ ~args ~resolve

  let obj = obj "user" ~fields:(fun _ -> [ id; name ])

  let field =
    let typ = non_null @@ list @@ non_null obj
    and args = Arg.[ arg "id" ~typ:int ]
    and resolve id' = List.find_opt (fun { id; _ } -> id = id') hardcoded in
    let resolve _ () = function
      | None -> hardcoded
      | Some id -> resolve id |> Option.to_list
    in
    field "users" ~typ ~args ~resolve
end

let () =
  let schema = Graphql_lwt.Schema.schema [ User.field ] in
  let default_query = "{\n  users {\n    id\n    name\n  }\n}" in
  let open Dream in
  let routing =
    [ any "/graphql" @@ graphql Lwt.return schema
    ; get "/" @@ graphiql ~default_query "/graphql"
    ]
  in
  router routing |> origin_referrer_check |> logger |> run
