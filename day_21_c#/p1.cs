  /*
enum Op {
  Mul,
  Add,
  Div,
  Sub
}
class OpUtil {
  public static Op OpFromStr(string op) {
    switch (op) {
      case "*":
        return Op.Mul;
      case "+":
        return Op.Add;
      case "/":
        return Op.Div;
      case "-":
        return Op.Sub;
    }
    throw new Exception("Invalid operation");
  }
  public static bool IsAddOrSub(Op op) {
    return op == Op.Add || op == Op.Sub;
  }
  public static Func<long, long, long> getOpFunc(Op op) {
    switch (op) {
      case Op.Mul:
        return (a, b) => checked(a * b);
      case Op.Add:
        return (a, b) => checked(a + b);
      case Op.Div:
        return (a, b) => checked(a / b);
      case Op.Sub:
        return (a, b) => checked(a - b);
    }
    throw new Exception("Invalid operation");
  }
}

enum NodeTy {
  Val,
  Expr
}
class Expr {
  public Node? left;
  public Node? right;
  public Op bop;
}
class Node {
  public NodeTy ty;
  public Expr? expr;
  public long? val;
  public Node(long val) {
    this.ty = NodeTy.Val;
    this.val = val;
  }
  public Node(Expr expr) {
    this.ty = NodeTy.Expr;
    this.expr = expr;
  }
  public static Node NodeFromEqStr(Dictionary <string, string[]> eqsStr, string id) {
    if (eqsStr[id].Count() == 1) {
      return new Node(long.Parse(eqsStr[id][0]));
    }
    return new Node(new Expr {
      left = Node.NodeFromEqStr(eqsStr, eqsStr[id][0]),
      right = Node.NodeFromEqStr(eqsStr, eqsStr[id][2]),
      bop = OpUtil.OpFromStr(eqsStr[id][1])
    });
  }

  public void Simplify() {
    if (this.ty == NodeTy.Expr && this.expr != null && this.expr.left != null && this.expr.right != null) {
      Expr expr = this.expr;
      Node left = expr.left;
      Node right = expr.right;
      Op bop = expr.bop;
      if (right.ty == NodeTy.Val && left.ty == NodeTy.Expr && left.expr != null && left.expr.left != null && left.expr.right != null) {
        Expr leftExpr = left.expr;
        Node leftLeft = leftExpr.left;
        Node leftRight = leftExpr.right;
        Op leftBop = leftExpr.bop;
        if (OpUtil.IsAddOrSub(bop) && OpUtil.IsAddOrSub(leftBop)) {
        }
      }
    }
  }

  public long Evaluate(int depth = 0) {
    if (this.ty == NodeTy.Val && this.val.HasValue) {
      return this.val.Value;
    }
    if (this.ty == NodeTy.Expr && this.expr != null && this.expr.left != null && this.expr.right != null) {
      Expr expr = this.expr;
      Node left = expr.left;
      Node right = expr.right;
      Op bop = expr.bop;
      return OpUtil.getOpFunc(bop)(left.Evaluate(), right.Evaluate());
    }
    throw new Exception("Invalid node");
  }
}

class P1 { public static void Main() {
  string[] lines = System.IO.File.ReadAllLines("input.txt");
  Dictionary<string, string[]> eqsStr = lines
    .ToDictionary(
      l => l.Split(":")[0].Trim(),
      l => l.Split(":")[1].Trim().Split(" ").Select(s => s.Trim()).ToArray()
    );
  Node root = Node.NodeFromEqStr(eqsStr, "root");
  Console.WriteLine(root.Evaluate());
} }
  */