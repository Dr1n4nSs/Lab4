open System

type Tree =
    | Empty
    | Node of int * Tree * Tree

module Tree =
    let rec map f tree =
        match tree with
        | Empty -> Empty
        | Node(v, left, right) ->
            Node(f v, map f left, map f right)

let rec generateRandomTree depth (rng: Random) =
    if depth <= 0 then Empty
    else
        let value = rng.Next(1, 100)
        Node(value, generateRandomTree (depth - 1) rng, generateRandomTree (depth - 1) rng)


let rec printTree indent tree =
    match tree with
    | Empty -> ()
    | Node(v, left, right) ->
        printTree (indent + "    ") right
        printfn "%s%d" indent v
        printTree (indent + "    ") left



[<EntryPoint>]
let main _ =
    let rng = Random()
    printf "\nВведите глубину  "
    let depth = Console.ReadLine()
    match Int32.TryParse(depth) with
    | (true, digit) ->
        let myTree = generateRandomTree digit rng
        Console.OutputEncoding <- System.Text.Encoding.UTF8

        printfn "Исходное дерево:"
        printTree "" myTree
    
        printf "\nВведите цифру для добавления в конец: "
        match Int32.TryParse(Console.ReadLine()) with
        | (true, digit) when digit >= 0 && digit <= 9 ->
            let newTree = myTree |> Tree.map (fun v -> v * 10 + digit)        
            printfn "\nТрансформированное дерево (добавлена цифра %d):" digit
            printTree "" newTree
        
        | _ -> printfn "Введена не цифра"
    | _ -> 
        printfn "Введена не цифра"    
    0