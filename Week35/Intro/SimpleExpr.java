// File Intro/SimpleExpr.java
// Java representation of expressions as in lecture 1
// sestoft@itu.dk * 2010-08-29

import java.util.Map;
import java.util.HashMap;

abstract class Expr { 
    abstract public int eval(Map<String,Integer> env);
    public boolean isZero() { return false; }
    public boolean isOne() { return false; }
    abstract public Expr simplify();
}

class CstI extends Expr { 
    final int i;

    public CstI(int i) { 
	this.i = i; 
    }
    
    public int eval(Map<String,Integer> env) {
	return i;
    } 
    @Override public boolean isZero() { return i == 0; }
    @Override public boolean isOne() { return i == 1; }

    public Expr simplify() {
	return this;
    }

    public String toString() {
	return "" + i;
    }
}

class Var extends Expr { 
    final String name;
    
    public Var(String name) { 
      this.name = name; 
    }
    
    public int eval(Map<String,Integer> env) {
      return env.get(name);
    } 

    public Expr simplify() {
	return this;
    }

    public String toString() {
	return name;
    }

}

abstract class Binop extends Expr { 
    protected final String oper;
    protected final Expr e1, e2;
    Binop(String oper, Expr e1, Expr e2) { 
	this.oper = oper; 
	this.e1 = e1;
	this.e2 = e2;
    }
    
    public String toString() {
	return "(" + e1.toString() + " " + oper + " " + e2.toString() + ")";
    }
}

class Add extends Binop {
    Add(Expr e1, Expr e2) { super("+", e1, e2); }
    public int eval(Map<String, Integer> env) {
	return e1.eval(env) + e2.eval(env);
    }
    public Expr simplify() {
	Expr l1 = e1.simplify();
	Expr l2 = e2.simplify();
	if (l1.isZero()) {
	    return l2;
	} else if (l2.isZero()) {
	    return l1;
	} else return this;
    }
}
class Mul extends Binop {
    Mul(Expr e1, Expr e2) { super("*", e1, e2); }
    public int eval(Map<String, Integer> env) {
	return e1.eval(env) * e2.eval(env);
    }
    public Expr simplify() {
	Expr l1 = e1.simplify();
	Expr l2 = e2.simplify();
	if (l1.isZero() || l2.isZero()) {
	    return new CstI(0);
	} else if (l1.isOne()) {
	    return l2;
	} else if (l2.isOne()) {
	    return l1;
	} else return this;
    }
}
class Sub extends Binop {
    Sub(Expr e1, Expr e2) { super("-", e1, e2); }
    public int eval(Map<String, Integer> env) {
	return e1.eval(env) - e2.eval(env);
    }
    public Expr simplify() {
	Expr l1 = e1.simplify();
	Expr l2 = e2.simplify();
	if (l1 instanceof CstI && l2 instanceof CstI) {
	    CstI c1 = (CstI) l1;
	    CstI c2 = (CstI) l2;
	    if (c1.i == c2.i) {
		return new CstI(0);
	    } else if (c2.isZero()) {
		return c1;
	    } else return this;
	} else return this;
    }
}


public class SimpleExpr {
  public static void main(String[] args) {
      // 1.4 (i)
      Expr e = new Add(new CstI(17), new Var("z"));
      System.out.println(e.toString());

      // 1.4 (ii)
      Expr e1 = new Mul(new Var("x"), new Sub(new CstI(12), new Var("y")));
      Expr e2 = new Mul(new Var("x"), new Sub(new CstI(12), new Var("y")));
      Expr e3 = new Sub(new Add(new CstI(14), new Var("m")), new CstI(2));
      
      // 1.4 (iii)
      Map<String, Integer> map = new HashMap<String, Integer>();
      map.put("m", 29);    
      System.out.println(e3.eval(map));
      
      // 1.4 (iv)
      Expr e4 = new Mul(new CstI(2), new CstI(0));
      Expr e5 = new Mul(new CstI(60), new CstI(1));
      Expr e6 = new Add(new CstI(2), new CstI(0));
      Expr e7 = new Sub(new CstI(2), new CstI(2));
      System.out.println(e4.simplify());
      System.out.println(e5.simplify());
      System.out.println(e6.simplify());
      System.out.println(e7.simplify());

  }
}
