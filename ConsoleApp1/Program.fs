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

let rec BTree (elements: int list) =
    match elements with
    | [] -> Empty
    | _ ->
        let midIndex = elements.Length / 2
        let leftPart = elements |> List.take midIndex
        let midValue = elements.[midIndex]
        let rightPart = elements |> List.skip (midIndex + 1)
        
        Node(midValue, BTree leftPart, BTree rightPart)

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
    match Int32.TryParse(Console.ReadLine()) with
    | (true, n) when n > 0 ->
        let sortedList = List.init n (fun _ -> rng.Next(1, 100))
                         |> List.sort
        
        let myTree = BTree sortedList

        printfn "\nИсходное дерево: "
        printTree "" myTree
    
        printf "\nВведите цифру для добавления в конец (0-9): "
        match Int32.TryParse(Console.ReadLine()) with
        | (true, digit) when digit >= 0 && digit <= 9 ->
            let newTree = myTree
                          |> Tree.map (fun v -> v * 10 + digit)        
            printfn "\nРезультат:"
            printTree "" newTree
        | _ -> printfn "Введена не цифра"
    | _ -> printfn "Некорректный ввод"
    
    0
