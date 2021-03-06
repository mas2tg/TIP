package tip.analysis

import tip.ast._
import tip.cfg.CfgOps._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData
import tip.solvers._
import tip.cfg._

/**
  * Base class for live variables analysis.
  */
abstract class LiveVarsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  val allVars: Set[ADeclaration] = cfg.nodes.flatMap(_.appearingIds)

  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allVars)) //TODO: how come we can pass cfg.nodes as the first parameter?

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          case cond: AExpr =>
            s ++ r.appearingIds
          case as: AAssignStmt =>
            as.left match {
              case id: AIdentifier =>
              s --  AstOps.AstOp(id).appearingIds ++ AstOps.AstOp(as.right).appearingIds // JOIN(v)\left union vars(right)
              case _ => ???
            }
          case varr: AVarStmt =>
            s -- varr.declIds // kill all variables in the beggining
          case ret: AReturnStmt =>
            s ++ r.appearingIds // JOIN(v) union vars(E)
          case out: AOutputStmt =>
            s ++ r.appearingIds // JOIN (v) union vars(E)
          case _ => s
        }
      case _ => s
    }
}

/**
  * Live variables analysis that uses the simple fixpoint solver.
  */
class LiveVarsAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with BackwardDependencies

/**
  * Live variables analysis that uses the worklist solver.
  */
class LiveVarsAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with BackwardDependencies
