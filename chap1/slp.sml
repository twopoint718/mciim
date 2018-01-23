type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm =
    CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list
    
and exp =
    IdExp of id
    | NumExp of int
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp

val prog =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStm(AssignStm("b",
            EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
                OpExp(NumExp 10, Times, IdExp"a"))),
        PrintStm[IdExp "b"]))

(* implementation exercises *)

fun max a b =
    if a > b then a else b

fun maximum xs =
    case xs of
      [] => 0
    | (hd :: tl) => max hd (maximum tl)

fun maxargs x =
    case x of
      CompoundStm (s1, s2) => max (maxargs s1) (maxargs s2)
    | AssignStm (_, expr) => maxargs_helper expr
    | PrintStm exprs =>
        max (length exprs) (maximum (map maxargs_helper exprs))

and maxargs_helper x =
    case x of
      IdExp _ => 1
    | NumExp _ => 1
    | OpExp (e1, _, e2) => 1
    | EseqExp (s, e) =>
        max (maxargs s) (maxargs_helper e)

(* Interpreter *)

exception NotFound

fun lookup ([], id) = raise NotFound
  | lookup (((k,v) :: tl), id) =
        if k = id then v else lookup (tl, id)

fun update (tbl, c, v) = (c, v) :: tbl

fun interpStm ((CompoundStm (s1, s2)), tbl) =
        let val tbl1 = interpStm (s1, tbl)
        in  interpStm (s2, tbl1)
        end
  | interpStm (AssignStm (n, e), tbl) =
        let val (i, tbl1) = interpExp (e, tbl)
        in  update (tbl1, n, i)
        end
  | interpStm (PrintStm es, tbl) =
        let fun f (expr, curr_tbl) =
                let val (i, new_tbl) = interpExp (expr, curr_tbl)
                in  print (Int.toString i ^ " ");
                    new_tbl
                end
            val new_tbl = foldl f tbl es
        in  print "\n"; (* newline after print statment *)
            new_tbl
        end

and interpExp (IdExp n, tbl) = (lookup (tbl, n), tbl)
  | interpExp (NumExp i, tbl) = (i, tbl)
  | interpExp (EseqExp (s, e), tbl) =
        let val new_tbl = interpStm (s, tbl)
        in  interpExp (e, new_tbl)
        end
  | interpExp (OpExp (e1, oper, e2), tbl) =
        let val (x, tbl1) = interpExp (e1, tbl)
            val (y, tbl2) = interpExp (e2, tbl1)
            val i = case oper of
                  Plus => x + y
                | Minus => x - y
                | Times => x * y
                | Div => x div y
        in  (i, tbl2)
        end

fun interp s = (interpStm (s, []); ())

(* and interpExp tbl  *)