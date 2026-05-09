let houses: int array = [|2; 7; 9; 3; 1|]

let house_robber (houses: int array): int =
    let n = Array.length houses in
    let rec aux i prev prev_prev =
        if i = n then
            prev
        else
            aux 
                (i + 1)
                (max (prev) (prev_prev + houses.(i))) 
                (prev)
    in
        aux 0 0 0

let () = 
    Printf.printf "%d" (house_robber (houses));
