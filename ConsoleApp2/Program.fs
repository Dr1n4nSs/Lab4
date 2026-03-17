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
    let rec fold f acc tree =
        match tree with
        | Empty -> acc
        | Node(v, left, right) ->
            let acc1 = f acc v
            let acc2 = fold f acc1 left
            fold f acc2 right      

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

let containsDigit (targetDigit: int) (value: int) =
    let targetStr = targetDigit.ToString()
    value.ToString().Contains(targetStr)

[<EntryPoint>]
let main _ =
    let rng = Random()
    
    printf "Введите количество узлов в дереве: "
    let input = Console.ReadLine()
    
    match Int32.TryParse(input) with
    | (true, n) ->
        let sortedList = List.init n (fun _ -> rng.Next(1, 100))
                         |> List.sort
        
        let myTree = BTree sortedList
        printfn "\nИсходное дерево:"
        printTree "" myTree
        
        printf "\nЦифра, которую НЕ должны содержать элементы: "
        let digitInput = Console.ReadLine()
        
        match Int32.TryParse(digitInput) with
        | (true, digit) when digit >= 0 && digit <= 9 ->
            let count = 
                myTree |> Tree.fold (fun acc v ->
                    if not (containsDigit digit v) then
                        acc + 1
                    else
                        acc
                ) 0

            printfn "\nРезультат:"
            printfn "Не содержит цифру %d: %d" digit count
            
        | _ -> printfn "Введена не цифра"
    | _ -> printfn "Введена не цифра"
    
    0
