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

let cikli p : int list -> int list list =
  let array_of_p = Array.of_list p
  id_array 
  let rec aux acc cikli k=
    function
    | [] -> List.rev cikli
    | _ -> if array_of_p.(k) 









      






























