class Exp {
  Exp() { super(); };
  
  Exp eval () { return this; }
  Exp applyto (Exp e) { return this; }
  String tostring () { return "<Exp>"; }
}

class App extends Exp {
  Exp f; Exp a;
  App (Exp f, Exp a) { super(); this.f=f; this.a=a; }
  Exp eval () { return f.eval().applyto(a).eval(); }
  Exp applyto (Exp e) { return new App(this, e); }
  String tostring () { 
    return "(" + this.f.tostring() + " " + this.a.tostring() + ")"; 
  }  
}

class K extends Exp {
  K () { super(); }
  Exp applyto (Exp e) { return new KM(e); }
  String tostring () { return "K"; }  
}

class KM extends Exp {
  Exp m;
  KM (Exp m) { super(); this.m=m; }
  Exp applyto (Exp e) { return m; }
  String tostring () { return "(K " + m.tostring() + ")"; }  
}

class S extends Exp {
  S () { super(); }
  Exp applyto (Exp e) { return new SM(e); }
  String tostring () { return "S"; }  
}

class SM extends Exp {
  Exp m;
  SM (Exp m) { super(); this.m=m; }
  Exp applyto (Exp e) { return new SMN(m,e); }
  String tostring () { return "(S " + m.tostring() + ")"; }  
}

class SMN extends Exp {
  Exp m; Exp n;
  SMN (Exp m, Exp n) { super(); this.m=m; this.n=n; }
  Exp applyto (Exp e) { return new App(new App(m,e), new App(n,e)); }
  String tostring () { return "(S "+ m.tostring()+" "+n.tostring()+")"; }
}

public class Test {
  public static void main (String[] args) {
    Exp k = new K();
    Exp s = new S();
    Exp sk = new App(s,k);
    Exp i = new App(sk,k);
    Exp e = new App(i,i);
    System.out.println(e.tostring() + " --> " + e.eval().tostring());
  }
}
