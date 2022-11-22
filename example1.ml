(*
  Basic regular expression matching algorithm using Brzozowski derivatives,
  shown on slides 13-15 on the handout.
*)

type regex =
  EmptySet
  |EmptyString
  |Character of char
  |Union of regex * regex
  |Concat of regex * regex
  |Star of regex
  |And of regex * regex
  |Not of regex

let rec nullable = function
  EmptySet -> false
  |EmptyString -> true
  |Character(_) -> false
  |Union(r, s) -> (nullable r) || (nullable s)
  |Concat(r, s) -> (nullable r) && (nullable s)
  |Star(_) -> true
  |And(r, s) -> (nullable r) && (nullable s)
  |Not(r) -> not (nullable r)

let rec derive r c =
  match r with
    EmptySet -> EmptySet
    |EmptyString -> EmptySet
    |Character(c') -> if c = c' then EmptyString
                      else EmptySet
    |Union(r, s) -> Union (derive r c, derive s c)
    |Concat(r, s) ->  if nullable r then
                        Union (Concat (derive r c, s), derive s c)
                      else
                        Concat(derive r c, s)
    |Star(r) -> Concat (derive r c,  Star(r))
    |And(r, s) -> And(derive r c, derive r c)
    |Not(r) -> Not(derive r c)

let regexmatch r char_list = nullable (List.fold_left derive r char_list)