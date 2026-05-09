let nums: int array = [|2; 3; -2; 4|]

let max_prod_subarray (nums: int array) : int =
    let n = Array.length nums in
    let rec aux i global_max current_max current_min =
        if (i = n) then
            global_max
        else
            let c1 = nums.(i) in
            let c2 = current_max * c1 in
            let c3 = current_min * c1 in
            let inner_max = max c1 (max c2 c3) in
            let inner_min = min c1 (min c2 c3) in
            aux
                (i + 1)
                (max global_max inner_max)
                (inner_max)
                (inner_min)
    in
        aux 1 nums.(0) nums.(0) nums.(0)

let () = 
    Printf.printf "%d" (max_prod_subarray (nums));
