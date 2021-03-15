namespace IsqGet.Functional

open System.Collections
open System.Collections.Generic

type Iterator<'T>(seq: 'T seq) =
    let enumerator = seq.GetEnumerator()
    
    member private _._enumerator = enumerator

    static member toSeq(iterator: Iterator<'T>): 'T seq =
        iterator :> IEnumerable<'T>
        
    static member next (iterator: Iterator<'T>): 'T option =
        if iterator._enumerator.MoveNext()
        then Some iterator._enumerator.Current
        else None
        
    static member tail (iterator: Iterator<'T>): Iterator<'T> =
        iterator |> Iterator.next |> ignore
        iterator       

    static member deconstruct(iterator: Iterator<'T>): ('T * Iterator<'T>) option =
        match iterator |> Iterator.next with
        | Some value -> Some (value, iterator)
        | None -> None
        
    interface IEnumerable<'T> with
        member this.GetEnumerator(): IEnumerator<'T> = this._enumerator
        member this.GetEnumerator(): IEnumerator = this._enumerator :> IEnumerator

module Iterator =
    let (|Deconstruct|Empty|) (iter: Iterator<'T>) =
        match iter |> Iterator.deconstruct with
        | Some (head, tail) -> Deconstruct(head, tail)
        | None -> Empty
