(* Generic list related *)

let evens (x: 'a list) : 'a list =
    let rec e (r: 'a list) (s: 'a list) : 'a list =
        match r with 
            | a::b::t -> e t (b::s)
            | _       -> s
    in List.rev (e x [])

let odds (x: 'a list) : 'a list =
    let rec o (r: 'a list) (s: 'a list) : 'a list =
        match r with 
            | a::b::t -> o t (a::s)
            | _       -> s
    in List.rev (o x [])
        
let double (x: 'a list) : 'a list =
    List.rev (List.rev_append x (List.rev x))

let half (x: 'a list) : 'a list =
    let l = (List.length x) / 2 in 
    let rec h (r: 'a list) (s: 'a list) (n: int) : 'a list =
        match r with 
            | a::b -> if   n < l 
                      then h b (a::s) (n+1)
                      else s
            | _    -> []
    in List.rev (h x [] 0)


(* Complex / complex list related *)

let complex_of_float (x: float) : Complex.t =
    { Complex.re = x ; Complex.im = 0. }

let complex_list_of_float_list (x: float list) : Complex.t list =
    List.rev (List.rev_map complex_of_float x)
    
let float_list_of_complex_list (x: Complex.t list) : float list = 
    List.rev (List.rev_map Complex.norm x)

let root (n: int) (d: int) : Complex.t =
    let pi = 3.1415926525 in
    let c  = 2. *. pi *. (float_of_int n) /. (float_of_int d) in
    { Complex.re = cos c ; Complex.im = sin c }

let fwd (x: Complex.t) (y: Complex.t) (n: int) (d: int) : Complex.t =
    Complex.add x (Complex.mul y (root (-1 * n) d))


(* Main mapping function *)

let fft_map (x:Complex.t list) (y: Complex.t list) (n: int) : Complex.t list =
    let rec m (x': Complex.t list) (y': Complex.t list) (r: Complex.t list) (s: int) (i: int) : Complex.t list =
        match (x',y') with 
            | (h::t, h'::t') -> m t t' ((fwd h h' i s)::r) s (i + 1)
            | (_,_)          -> r
    in List.rev (m x y [] n 0) 


(* FFT *)

let rec fft (x: Complex.t list) : Complex.t list =
    let s  = List.length x in
    match x with
        | [a] -> [a]
        | _   -> let e = double (fft (evens x)) in
                 let o = double (fft (odds  x)) in
                 fft_map e o s

let real_fft (x: float list) : float list =
    let x' = complex_list_of_float_list x in
    let y  = fft x' in 
    let y' = float_list_of_complex_list y in 
    half y'
    
    
(* Example *)    

let _ = real_fft [1.0;2.0;3.0;4.0]
