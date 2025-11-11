(*----------------------------------------------------------------------------*
 # 1. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Ogrevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Collatzovo zaporedje
[*----------------------------------------------------------------------------*)

let collatz n =
  if n <= 0 then invalid_arg "Začetni člen mora biti pozitiven!";
  let je_sodo n = n mod 2 = 0 in
  let rec aux acc i =
    let acc' = i :: acc in
    match i with
    | 1 -> List.rev acc'
    | _ ->
      if je_sodo i then aux acc' (i / 2)
      else aux acc' (3 * i + 1)
  in
  aux [] n;;

(*----------------------------------------------------------------------------*
 ### Fiksne točke
[*----------------------------------------------------------------------------*)

let fiksne_tocke funkcija seznam = 
  let rec aux acc =
    function
    | [] -> List.rev acc
    | glava :: rep -> 
      if funkcija glava = glava then aux (glava :: acc) rep
      else aux acc rep
  in
  aux [] seznam;;

(*----------------------------------------------------------------------------*
 ### Združevanje z ločilom
[*----------------------------------------------------------------------------*)

let sep_concat locilo seznam_seznamov =
  let rec dodaj_podseznam acc =
    function
    | [] -> acc 
    | glava :: rep -> dodaj_podseznam (glava :: acc) rep
  in
  
  let rec aux acc =
    function
    | []-> List.rev (locilo :: acc)
    | podseznam :: ostalo ->
      let dodaj_locilo = locilo :: acc in
      let z_locili = dodaj_podseznam dodaj_locilo podseznam in
      aux z_locili ostalo
  in
  aux [] seznam_seznamov;;

(*----------------------------------------------------------------------------*
 ### Razbitje seznama
[*----------------------------------------------------------------------------*)

let rec partition k seznam =
  if k <= 0 then invalid_arg "k mora biti pozitiven!"
  else
    let rec aux acc podseznami n =
      function
      | [] -> 
        (match acc with
          | [] -> List.rev podseznami
          | _ -> List.rev ((List.rev acc) :: podseznami))
      | glava :: rep ->
        if n < k then
          aux (glava :: acc) podseznami (n + 1) rep
        else
          aux [glava] ((List.rev acc) :: podseznami) 1 rep
    in
    aux [] [] 0 seznam;;
    
          
(*----------------------------------------------------------------------------*
 ### Izomorfizmi množic
[*----------------------------------------------------------------------------*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

let phi1 (a, b) = (b, a)
let psi1 (b, a) = (a, b)

let phi2 = 
  function
  | In1 a -> In2 a
  | In2 b -> In1 b
let psi2 =
  function
  | In2 a -> In1 a
  | In1 b -> In2 b

let phi3 (a, (b, c)) = ((a, b), c)
let psi3 ((a,b), c) = (a, (b, c))

let phi4 =
  function
  | In1 a -> In1 (In1 a)
  | In2 (In1 b) -> In1 (In2 b)
  | In2 (In2 c) -> In2 c

let psi4 =
  function
  | In1 (In1 a) -> In1 a
  | In1 (In2 b) -> In2 (In1 b)
  | In2 c -> In2 (In2 c)

let phi5 (a, sum_bc) =
  match sum_bc with
  | In1 b -> In1 (a, b)
  | In2 c -> In2 (a, c)
let psi5 sum_a_bc =
  match sum_a_bc with
  | In1 (a, b) -> (a, In1 b)
  | In2 (a, c) -> (a, In2 c)

let phi6 f =
  let gb b = f (In1 b) in
  let gc c = f (In2 c) in
  (gb, gc)

let psi6 (gb, gc) =
  function
  | In1 b -> gb b
  | In2 c -> gc c

let phi7 f = 
  ((fun c -> fst (f c)), (fun c -> snd (f c)))

let psi7 (ga, gb) c = (ga c, gb c) 

(*----------------------------------------------------------------------------*
 ## Permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Kompozitum
[*----------------------------------------------------------------------------*)

let kompozitum p q : int list =
  List.map (Array.get (Array.of_list q)) p

(*----------------------------------------------------------------------------*
 ### Inverz
[*----------------------------------------------------------------------------*)

let inverz p =
  let rec aux acc k =
    function
    | [] -> Array.to_list acc
    | glava :: rep -> 
      acc.(glava) <- k;
      aux acc (k + 1) rep
  in
  aux (Array.make (List.length p) 0) 0 p;;

(*----------------------------------------------------------------------------*
 ### Razcep na cikle
[*----------------------------------------------------------------------------*)

let cikli p =
  let dolzina = List.length p in
  let p_array = Array.of_list p in
  let obiskani = Array.make dolzina false in
  let rec nov_cikel acc k =
    if obiskani.(k) then List.rev acc
    else (
      obiskani.(k) <- true;
      nov_cikel (k :: acc) p_array.(k)
    )
  in
  let rec aux seznam_ciklov k =
    if k = dolzina then List.rev seznam_ciklov
    else if obiskani.(k) then aux seznam_ciklov (k + 1)
    else 
      aux ((nov_cikel [] k) :: seznam_ciklov) (k + 1)
  in
  aux [] 0;;


(*----------------------------------------------------------------------------*
 ### Transpozicije permutacije
[*----------------------------------------------------------------------------*)

let iz_transpozicij dolzina transpozicije =
  let permutacija_array = Array.init dolzina (fun i -> i) in
  let rec aux =
    function
    | [] -> permutacija_array |> Array.to_list
    | (i, j) :: rep -> 
      let shrani_v_i = permutacija_array.(i) in
      permutacija_array.(i) <- permutacija_array.(j);
      permutacija_array.(j) <- shrani_v_i;
      aux rep
  in
  aux transpozicije;;

  let v_transpozicije permutacija =
    let rec transpozicije_cikla =
      function
      | [] -> []
      | glava :: rep -> 
        let rec aux acc =
          function
          | [] -> List.rev acc
          | glava2 :: rep -> aux ((glava, glava2) :: acc) rep
        in
        aux [] rep
    in
    let rec cikli_v_transpozicije acc =
      function
      | [] -> List.rev acc
      | cikel :: rep -> cikli_v_transpozicije (List.append (transpozicije_cikla cikel) acc) rep
    in
    cikli_v_transpozicije [] (cikli permutacija);;



(*----------------------------------------------------------------------------*
 ## Sudoku
[*----------------------------------------------------------------------------*)

type mreza = int option array array
type resitev = int array array

let primer_mreze : mreza = [|
  [|Some 5; Some 4; None;   None;   Some 7; None;   None;   None;   None|];
  [|Some 6; None;   None;   Some 1; Some 9; Some 5; None;   None;   None|];
  [|None;   Some 9; Some 8; None;   None;   None;   None;   Some 6; None|];
  [|Some 8; None;   None;   None;   Some 6; None;   None;   None;   Some 3|];
  [|Some 4; None;   None;   Some 8; None;   Some 3; None;   None;   Some 1|];
  [|Some 7; None;   None;   None;   Some 2; None;   None;   None;   Some 6|];
  [|None;   Some 6; None;   None;   None;   Some 7; Some 8; None;   None|];
  [|None;   None;   None;   Some 4; Some 1; Some 9; None;   None;   Some 5|];
  [|None;   None;   None;   None;   Some 8; None;   None;   Some 7; Some 9|]
|]

let primer_resitve : resitev = [|
  [|5; 4; 3; 6; 7; 8; 9; 1; 2|];
  [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
  [|1; 9; 8; 3; 4; 2; 5; 6; 7|];
  [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
  [|4; 2; 6; 8; 5; 3; 7; 9; 1|];
  [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
  [|9; 6; 1; 5; 3; 7; 8; 2; 4|];
  [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
  [|3; 5; 4; 2; 8; 6; 1; 7; 9|];
|]

(*----------------------------------------------------------------------------*
 ### Dopolnitev mreže
[*----------------------------------------------------------------------------*)

let dodaj i j n m =
  let nova_mreza = Array.map Array.copy m in
  nova_mreza.(i).(j) <- Some n;
  nova_mreza



(*----------------------------------------------------------------------------*
 ### Izpiši mrežo
[*----------------------------------------------------------------------------*)

let izpis_mreze mreza  =
  let izpis_celice celica =
    match celica with
    | None -> "."
    | Some n -> string_of_int n
  in
  let izpis_vrstice vrstica =
    let rec aux acc k =
      function
      | [] -> String.concat "" (List.rev ("|\n" :: acc))
      | celica :: rep ->
        let acc = if k mod 3 = 0 then "| " :: acc else acc in
        let acc = " " :: izpis_celice celica :: acc in
        aux acc (k + 1) rep
    in
    aux [] 0 (Array.to_list vrstica)
  in
  let ogrodje = "+-------+-------+-------+\n" in
  let rec aux acc k =
    function
    | [] -> String.concat "" (List.rev (ogrodje :: acc))
    | vrstica :: rep ->
      let acc = if k mod 3 = 0 then ogrodje :: acc else acc in
      let acc = izpis_vrstice vrstica :: acc in
      aux acc (k + 1) rep
  in
  aux [] 0 (Array.to_list mreza) 

let izpis_resitve resitev =
    let izpis_vrstice vrstica =
    let rec aux acc k =
      function
      | [] -> String.concat "" (List.rev ("|\n" :: acc))
      | celica :: rep ->
        let acc = if k mod 3 = 0 then "| " :: acc else acc in
        let acc = " " :: string_of_int celica :: acc in
        aux acc (k + 1) rep
    in
    aux [] 0 (Array.to_list vrstica)
  in
  let ogrodje = "+-------+-------+-------+\n" in
  let rec aux acc k =
    function
    | [] -> String.concat "" (List.rev (ogrodje :: acc))
    | vrstica :: rep ->
      let acc = if k mod 3 = 0 then ogrodje :: acc else acc in
      let acc = (izpis_vrstice vrstica) :: acc in
      aux acc (k + 1) rep
  in
  aux [] 0 (Array.to_list resitev) 

(*----------------------------------------------------------------------------*
 ### Preveri ali rešitev ustreza mreži
[*----------------------------------------------------------------------------*)

let ustreza (mreza : mreza) (resitev : resitev) : bool =
  let st_vrstic = Array.length mreza and st_stolpcev = Array.length mreza.(0) in
  let rec loop_i i =
    if i = st_vrstic then true
    else
      let rec loop_j j =
        if j = st_stolpcev then true
        else
          match mreza.(i).(j) with
          | None -> loop_j (j + 1)
          | Some n ->
            if resitev.(i).(j) = n then loop_j (j + 1)
            else false
    in
    if loop_j 0 then loop_i (i + 1) else false
  in
  loop_i 0;;


(*----------------------------------------------------------------------------*
 ### Kandidati za dano prazno mesto
[*----------------------------------------------------------------------------*)
let ni_v_vrstici (mreza, i) n =
  let rec aux j =
    if j = Array.length mreza.(i) then true
    else
      match mreza.(i).(j) with
      | Some m -> if m = n then false else aux (j + 1)
      | None -> aux (j + 1)
    in
    aux 0;;

  let ni_v_stolpcu (mreza, j) n =
    let rec aux i =
    if i = Array.length mreza then true
    else
      match mreza.(i).(j) with
      | Some m -> if m = n then false else aux (i + 1)
      | None -> aux (i + 1)
    in
    aux 0;;
  
    let ni_v_skatli (mreza, k) n =
      let zacetni_i = (k / 3) * 3 and zacetni_j = (k mod 3) * 3 in
      let koncni_i = zacetni_i + 3 and koncni_j = zacetni_j + 3 in
      let rec aux i j =
        if i = koncni_i then true
        else if j = koncni_j then aux (i + 1) zacetni_j
        else
          match mreza.(i).(j) with
          | Some m -> if m = n then false else aux i (j + 1)
          | None -> aux i (j + 1)
        in
        aux zacetni_i zacetni_j;;
      
    let kandidati mreza vrstica stolpec =
      match mreza.(vrstica).(stolpec) with
      | Some _ -> None
      | None -> 
        let vsi_kandidati = [1;2;3;4;5;6;7;8;9] in
        let skatla = (vrstica / 3) * 3 + (stolpec / 3) in
        let rec aux acc =
          function
          | [] -> Some (List.rev acc)
          | glava :: rep ->
            if ni_v_vrstici (mreza, vrstica) glava &&
               ni_v_stolpcu (mreza, stolpec) glava &&
               ni_v_skatli (mreza, skatla) glava then
              aux (glava :: acc) rep
            else aux acc rep
        in
        aux [] vsi_kandidati;;

(*----------------------------------------------------------------------------*
 ### Iskanje rešitve
[*----------------------------------------------------------------------------*)

let resi mreza =
  let celica_z_najmanj_kandidati mreza =
    let rec aux_i i =
      if i = Array.length mreza then None
      else
        let rec aux_j j =
          if j = Array.length mreza.(i) then None
          else
            match kandidati mreza i j with
            | None -> aux_j (j + 1)
            | Some seznam_kandidatov ->
              match aux_j (j + 1) with
              | None -> Some (i, j, seznam_kandidatov)
              | Some (i_min, j_min, min_kandidati) -> 
                if List.length seznam_kandidatov < List.length min_kandidati then
                  Some (i, j, seznam_kandidatov)
                else
                  Some (i_min, j_min, min_kandidati)
        in
        match aux_j 0 with
        | None -> aux_i (i + 1)
        | Some (i_min, j_min, min_kandidati) -> Some (i_min, j_min, min_kandidati)
      in
      aux_i 0
    in
    let rec aux mreza =
      match celica_z_najmanj_kandidati mreza with
      | None -> (* ni mi se uspelo dokoncati, zal *) 
      | Some (i, j, seznam_kandidatov) ->
        let rec poskusi_kandidate =
          function
          | [] -> None
          | glava :: rep ->
            let nova_mreza = dodaj i j glava mreza in
            match aux nova_mreza with
            | None -> poskusi_kandidate rep
            | Some resitev -> Some resitev
          in
          poskusi_kandidate seznam_kandidatov
      in
      aux mreza;;
      
