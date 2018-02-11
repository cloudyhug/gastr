(* The target string *)
let _perfect = "Some may question your right to destroy ten billion people. Those who understand realise that you have no right to let them live !"
in

(* Returns a printable character (from Space character to DEL character) *)
let random_char () = Char.chr ((Random.int 97) + 32)
in

(* Makes a random individual of length 'len' *)
let make_individual len =
    let ind = Bytes.create len in
    for i = 0 to len - 1 do
        Bytes.set ind i (random_char());
    done;
    ind
in

(* Makes a crossover between 'ind1' and 'ind2' individuals *)
let rec cross ind1 ind2 len =
    let ind = Bytes.create len in
    let i = Random.int len in
    
    for k = 0 to i do
        Bytes.set ind k (Bytes.get ind1 k);
    done;
    
    for k = i + 1 to len - 1 do
        Bytes.set ind k (Bytes.get ind2 k);
    done;
    ind
in

(* Computes the error between 'str' and the target *)
let error str =
    let counter = ref 0 in
    for i = 0 to String.length _perfect - 1 do
        if (str.[i] <> _perfect.[i]) then incr counter
    done;
    !counter
in

(* Mutates an individual.
 * There are no mutations if the individual is already perfect.
 * There are 4 mutations if the individual is really far from the target.
 * Otherwise one mutation is done with a 25% probability.
 *)
let mutate ind len =
    let rec mutate_aux n =
        for k = 0 to n - 1 do
            let i = Random.int len in
            Bytes.set ind i (random_char());
        done
    in
    if (error ind = 0) then () else
    if (error ind > String.length _perfect / 2) then mutate_aux 4 else
    if (Random.int 100 > 75) then mutate_aux 1
in

(* Makes a list with 'nb' random individuals of length 'len' *)
let make_random_inds nb len =
    let rec aux_random_inds i n acc =
        if (i = n) then acc
        else aux_random_inds (i+1) n ((make_individual len)::acc)
    in aux_random_inds 0 nb []
in

(* Sorts the generation (lowest error scores are ranked first) *)
let sort_gen gen =
    let gentuples = List.map (fun ind -> (ind, error ind)) gen in
    let rec qsort l = match l with
        | [] -> []
        | gt::r ->
            let x = snd gt in
            let minus = List.filter (fun (i, ei) -> ei < x) r in
            let plus = List.filter (fun (i, ei) -> ei >= x) r in
            (qsort minus)@(gt::(qsort plus))
    in List.map fst (qsort gentuples)
in

(* Returns a list with the 'n' first elements of the list 'l' *)
let take n l =
    let rec take_aux i n l acc = if (i = n) then acc else match l with
        | [] -> acc
        | e::r -> take_aux (i+1) n r (e::acc)
    in take_aux 0 n l []
in

(** Composition of generation n+1 :
 * the best individual from generation n
 * 80% are the best 20% of generation n, crossed
 * the rest are random individuals
 * All the individuals may mutate.
 *)
let make_next_gen sortedgen nb len =
    let nb_cross_generated = truncate (0.80 *. (float nb)) in
    let nb_best_crossed = truncate (0.20 *. (float nb)) in
    let nb_random = nb - 1 - nb_cross_generated in
    
    let rec make_random_crosses inds i n acc =
        if (i = n) then acc else
        let a = Random.int (List.length inds) and b = Random.int (List.length inds) in
        if (a = b) then make_random_crosses inds i n acc
        else make_random_crosses inds (i+1) n ((cross (List.nth inds a) (List.nth inds b) len)::acc)
    in
    
    let next1 = List.hd sortedgen in
    let next2 = make_random_crosses (take nb_best_crossed sortedgen) 0 nb_cross_generated [] in
    let next3 = make_random_inds nb_random len in
    
    let result = next1::(next2 @ next3) in
    
    let rec mutate_all g = match g with
        | [] -> ()
        | e::r ->
            mutate e len;
            mutate_all r
    in
    mutate_all result;
    sort_gen result
in

(* Main loop *)

let main () =
    Random.self_init();
    let perflength = String.length _perfect in
    let generation = ref (sort_gen (make_random_inds 200 perflength)) in
    let e = ref 1 in
    let i = ref 0 in
    while !e > 0 do
        generation := make_next_gen !generation 200 perflength;
        incr i;
        let best = Bytes.to_string (List.hd !generation) in
        e := error best;
        Printf.printf "Generation %d | Best : %s | Error : %d\n" !i best !e;
    done
in

main();