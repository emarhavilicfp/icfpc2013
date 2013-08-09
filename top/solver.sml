signature SOLVER =
sig
  type spec = {size: int, ops: BV.operator list}
  type solver = {shortname: string, name: string, f: spec -> unit}
  
  val TotalNobe : solver
end

structure Solver : SOLVER =
struct
  type spec = {size: int, ops: BV.operator list}
  type solver = {shortname: string, name: string, f: spec -> unit}
  
  val TotalNobe : solver = let
    fun solve spec = ()
  in
    { shortname = "totalnobe", name = "Total nobe", f = solve }
  end
end

