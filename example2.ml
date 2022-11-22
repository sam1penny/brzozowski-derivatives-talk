(*
  A more sophisticated method for DFA construction from a regular expression,
  using similar-canonical form.
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

let union (r, s) =
  match r,s with
    |_, _ when r = s -> r
    |EmptySet, _ -> s
    |_, EmptySet -> r
    |Not(EmptySet), _ -> r
    |_, Not(EmptySet) -> s
    |_ -> if r < s then Union(r, s)
          else Union(s, r)

let concat (r, s) =
  match r, s with
    |_, _ when r = s -> r
    |EmptySet, _ -> r
    |_, EmptySet -> s
    |EmptyString, _ -> s
    |_, EmptyString -> r
    |Concat(_,_), _ -> Concat(s, r)
    |_ -> if r < s then Concat(r, s)
    else Concat(s, r)

let star r =
  match r with
    EmptySet | EmptyString -> EmptyString
    |Star(_) -> r
    |_ -> Star (r)

let and_r (r, s) =
  match r,s with
    EmptySet, _ -> r
    |_, EmptySet -> s
    |Not(EmptySet), _ -> s
    |_, Not(EmptySet) -> r
    |_ -> And(r, s)


let not_r r =
  match r with
    Not(r') -> r'
    |_ -> Not(r)


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
    |Union(r, s) -> union (derive r c, derive s c)
    |Concat(r, s) ->  if nullable r then
      union (concat (derive r c, s), derive s c)
    else
      concat(derive r c, s)
    |Star(r) -> concat (derive r c,  star(r))
    |And(r, s) -> and_r(derive r c, derive r c)
    |Not(r) -> not_r(derive r c)

let regexmatch r char_list = nullable (List.fold_left derive r char_list)

type transition_relation = (regex * char) * regex list
type dfa = regex list * regex * regex list * transition_relation
type alphabet = char list

let rec alphabet_helper acc = function
    |EmptySet | EmptyString -> acc
    |Character(c) -> if List.exists (fun c' -> c = c') acc then acc else c::acc
    |Union(r, s) -> alphabet_helper (alphabet_helper acc r) s
    |Concat(r, s) -> alphabet_helper (alphabet_helper acc r) s
    |Star(r) -> alphabet_helper acc r
    |And(r, s) -> alphabet_helper (alphabet_helper acc r) s
    |Not(r) -> alphabet_helper acc r

let determine_alphabet = alphabet_helper []

let rec goto alphabet q (states, transition_relation) c  =
  let qc = derive q c in
  match List.find_opt (fun q' -> q' = qc) states with
    Some(q') -> (states, ((q, c), q') :: transition_relation)
    |None -> let states' = qc :: states in
              let transition_relation' = ((q, c), qc) :: transition_relation in
             explore alphabet (states', transition_relation', qc)
and
explore alphabet (states, transition_relation, q) = List.fold_left (goto alphabet q) (states, transition_relation) alphabet

let mkDFA r alphabet =
  let q0 = r in
  let (states, transition_relation) = explore alphabet ([q0], [], q0) in
  let accept_states = List.filter (fun q -> nullable q) states in
  states, q0, accept_states, transition_relation

let example_regex = union (concat(Character('a'), Character('b')),concat(Character('a'), Character('c')))
let example_alphabet = determine_alphabet example_regex