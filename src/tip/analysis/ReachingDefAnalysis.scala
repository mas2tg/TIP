package tip.analysis

import tip.ast
import tip.ast._
import tip.cfg.CfgOps._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData
import tip.solvers._
import tip.cfg._

abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  val allVars: Set[ADeclaration] = cfg.nodes.flatMap(_.appearingIds)
  val allLocalsAndParams: Set[ADeclaration] = cfg.nodes.flatMap(_.declaredVarsAndParams)
  val allAssignments: Set[AAssignStmt] = cfg.nodes.flatMap(_.appearingAssignments)
  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allAssignments))

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case assign: AAssignStmt => // must be an assignment statement
            assign.left match {
              case aid: AIdentifier => // left side must be an identifier
                s.filterNot(e => // get rid of assignments that contain assigned variable
                  e.left match {
                    case bid: AIdentifier => // e.left better be an AIdentifier
                      aid.name == bid.name // if their names matches
                    case _ => false // default value
                  }) ++ Set(assign) // include the existing assignment
            //s.filterNot(e => e.left.name == aid.name) ++ Set(assign)
              case _ => s
            }
            //s.filterNot(e => e.left == assign.left) ++ Set(assign) //TODO: need to look up declaration as opposed to name only

          case _ => s
        }
      case _ => s
    }
}
class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
  extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
  extends ReachingDefAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
