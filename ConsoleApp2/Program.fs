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

let containsDigit (targetDigit: int) (value: int) =
    let targetStr = targetDigit.ToString()
    value.ToString().Contains(targetStr)

[<EntryPoint>]
let main _ =
    let rng = Random()
    Console.OutputEncoding <- System.Text.Encoding.UTF8
    
    printf "Введите глубину дерева: "
    let depthInput = Console.ReadLine()
    
    match Int32.TryParse(depthInput) with
    | (true, depth) ->
        let myTree = generateRandomTree depth rng
        printfn "\nИсходное дерево:"
        printTree "" myTree
        
        printf "\nВведите цифру, которую НЕ должны содержать элементы: "
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
            printfn "Количество элементов, не содержащих цифру %d: %d" digit count
            
        | _ -> printfn "Введена не цифра"
    | _ -> printfn "Введена не цифра"
    
    0