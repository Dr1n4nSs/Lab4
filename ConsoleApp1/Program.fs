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

    let rec insert x tree =
        match tree with
        | Empty -> Node(x, Empty, Empty)
        | Node(v, left, right) ->
            if x < v then Node(v, insert x left, right)
            else Node(v, left, insert x right)

let generateSearchTree count (rng: Random) =
    let rec loop n acc =
        if n <= 0 then acc
        else loop (n - 1) (Tree.insert (rng.Next(1, 100)) acc)
    loop count Empty

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

    printf "Введите количество узлов в дереве: "
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | (true, n) when n >= 0 ->
        let myTree = generateSearchTree n rng

        printfn "\nИсходное бинарное дерево поиска:"
        printTree "" myTree
    
        printf "\nВведите цифру для добавления в конец (0-9): "
        match Int32.TryParse(Console.ReadLine()) with
        | (true, digit) when digit >= 0 && digit <= 9 ->
            let newTree = myTree |> Tree.map (fun v -> v * 10 + digit)        
            printfn "\nТрансформированное дерево (добавлена цифра %d):" digit
            printTree "" newTree
        | _ -> printfn "Введена не цифра"
    | _ -> 
        printfn "Некорректное количество элементов"    
    0
