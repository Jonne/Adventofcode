let input = seq {1;0;0;3;1;1;2;3;1;3;4;3;1;5;0;3;2;6;1;19;1;5;19;23;2;9;23;27;1;6;27;31;1;31;9;35;2;35;10;39;1;5;39;43;2;43;9;47;1;5;47;51;1;51;5;55;1;55;9;59;2;59;13;63;1;63;9;67;1;9;67;71;2;71;10;75;1;75;6;79;2;10;79;83;1;5;83;87;2;87;10;91;1;91;5;95;1;6;95;99;2;99;13;103;1;103;6;107;1;107;5;111;2;6;111;115;1;115;13;119;1;119;2;123;1;5;123;0;99;2;0;14;0}

let replace list index value =
    list |> Seq.mapi(fun i e -> if i <> index then e else value) 

let add list pos1 pos2 dest = 
    let sum = Seq.item pos1 list + Seq.item pos2 list
    replace list dest sum
    
let multiply list pos1 pos2 dest = 
    let mult = Seq.item pos1 list * Seq.item pos2 list
    replace list dest mult

let rec compile list pos =
    let command = Seq.item pos list   

    if (command = 99) then 
        list
    else
        let param1 = Seq.item (pos+1) list 
        let param2 = Seq.item (pos+2) list
        let param3 = Seq.item (pos+3) list    
        match command with
        | 1 -> compile (add list param1 param2 param3) (pos+4)
        | 2 -> compile (multiply list param1 param2 param3) (pos+4)     

let calculate noun verb = 
    let restoredInput = replace (replace input 1 noun ) 2 verb
    let output = compile restoredInput 0
    Seq.head output 

let result = calculate 12 2

printf "%A" ( result)

let generateCombinations n : seq<int * int> = 
    let range = seq{0..n} 
    range |> Seq.collect(fun i -> range |> Seq.map(fun j -> (i, j)))

let (noun, verb) = generateCombinations 99 |> Seq.find(fun ((x, y)) -> (calculate x y) = 19690720)

let answer = 100 * noun + verb


