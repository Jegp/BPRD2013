// File Intro/SimpleExpr.java
// Java representation of expressions as in lecture 1
// sestoft@itu.dk * 2010-08-29

import java.util.Map;
import java.util.HashMap;

abstract class Expr { 
  abstract public int eval(Map<String,Integer> env);
  abstract public Expr simplify();
  abstract public String toString();
}

class CstI extends Expr { 
  protected final int i;

  public CstI(int i) { 
    this.i = i; 
  }

  public int eval(Map<String,Integer> env) {
    return i;
  } 

  public Expr simplify() {return this;}
  public String toString() {return Integer.toString(i);}
}

class Var extends Expr { 
  protected final String name;

  public Var(String name) { 
    this.name = name; 
  }

  public int eval(Map<String,Integer> env) {
    return env.get(name);
  } 

  public Expr simplify() {return this;}
  public String toString() {return name;}
}

class Prim extends Expr {
  protected final String oper;
  protected final Expr e1, e2;

  public Prim(String oper, Expr e1, Expr e2) { 
    this.oper = oper; this.e1 = e1; this.e2 = e2;
  }

  public int eval(Map<String,Integer> env) {
    if (oper.equals("+"))
      return e1.eval(env) + e2.eval(env);
    else if (oper.equals("*"))
      return e1.eval(env) * e2.eval(env);
    else if (oper.equals("-"))
      return e1.eval(env) - e2.eval(env);
    else
      throw new RuntimeException("unknown primitive");
  }

  public Expr simplify() { //still immutable, which isn't very oop, but..
    Expr ev1 = e1.simplify(), ev2 = e2.simplify();
    if (oper.equals("+")){
      if (ev1.toString().equals("0"))
        return ev2;
      if (ev2.toString().equals("0"))
        return ev1;
    } else if (oper.equals("*")){
      if (ev1.toString().equals("0") || ev2.toString().equals("0"))
        return new CstI(0);
      if (ev1.toString().equals("1"))
        return ev2;
      if (ev2.toString().equals("1"))
        return ev1;
    } else if (oper.equals("-")) {
      if (ev1.toString().equals("0"))
        return ev2;
      if (ev2.toString().equals("0"))
        return ev1;
    }
    return this;
  }

  public String toString() {
    return "(" + e1.toString() +
      " " + oper + " " + e2.toString() + ")";
  }
}

public class SimpleExpr {
  public static void main(String[] args) {
    Expr e1 = new CstI(17);
    Expr e2 = new Prim("+", new CstI(3), new Var("a"));
    Expr e3 = new Prim("+", new Prim("*", new Var("b"), new CstI(9)),
      new Var("a"));
    Expr e4 = new Prim("-", e3, new Var("baf"));

    Map<String,Integer> env0 = new HashMap<String,Integer>();
    env0.put("a", 3);
    env0.put("c", 78);
    env0.put("baf", 666);
    env0.put("b", 111);
    System.out.println(e1.eval(env0));
    System.out.println(e2.eval(env0));
    System.out.println(e3.eval(env0));
    System.out.println(e4.eval(env0));
    System.out.println(new Prim("*", new Var ("x"),
          new Prim("+", new CstI(0),
            new CstI(1))).simplify());
  }
}

// vim: set ts=2 shiftwidth=2 expandtab:
