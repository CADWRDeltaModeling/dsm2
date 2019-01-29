package interpolator;

/**
 * <p>Title: Interpolator</p>
 * <p>Description: Interpolation routines</p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author Eli Ateljevich
 * @version 1.0
 */
import java.io.*;
import IMSL.RootFinding;
public class ConservativeSpline {


  static double[] x;          // abscissa values
  private double[] a;         // fit parameter
  private double[] b;         // fit parameter
  private double[] c;         // fit parameter
  private static double[] d;
  private static double[] e;
  private double[] p;         // adjustment parameter p > -1
  private double[] q;          // adjustment parameter q > -1 (currently p=q enforced)
  private boolean pos;
  private static final double EPS = 1E-12;
  private static final double DEFAULT_LBOUND = -0.5*Double.MAX_VALUE;
  private static final double MAX_DATA_VALUE = 0.5*Double.MAX_VALUE;
  private static double[] y1;

  private int n=0;
  private int indx = 0;    // placeholder for when spline is evaluated

  /**
   * This is the main constructor, and assumes that the full set of input parameters
   * is provided, including the initial and terminal conditions and minimum bound
   * @param xval abscissas at which data are provided
   * @param y    original data. y[k] represents the (average) valued from x[k] to x[k+1]
   * @param pq   array of values for the tension parameters p and q (constrained to be the same)
   * @param y0   initial condition
   * @param yn   terminal condition
   * @param ymin minimum value for y
   * @throws Exception
   */
  public ConservativeSpline(double[] xval, double[] y, double[] pq,
                  double y0, double yn, double ymin) throws IllegalArgumentException {
    x=xval;
    p=pq;
    q=pq;
    n=x.length;
    if (n<=3) throw new IllegalArgumentException("Array arguments to the spline must have more than 3 elements");
    if (y.length != n | p.length != n | q.length !=n){
        throw new IllegalArgumentException("Array arguments to the spline must all be the same length");
        }
     //
    // Check values of parameters and fitted values for range

    if (y0 < ymin | yn <ymin){ throw new IllegalArgumentException("y0 or yn must be >= ymin"); }
    int i=0;
    while (i<n){
      //for (int i=0; i<n; i++){
      if (p[i] <= EPS-1 | q[i] <= EPS-1) throw new IllegalArgumentException(
        "Parameters p,q must be > eps-1");
      if (y[i] < ymin | y[i]>MAX_DATA_VALUE) throw new IllegalArgumentException(
        "Data value out of reasonable bounds");
      if (y[i] == ymin){
        nn=i;}

    a = new double[n];
    b = new double[n];
    c = new double[n];
    boolean boundsSatisfied = false;
    while(! boundsSatisfied ){
        try{
           fitHistSpline(y,y0,yn);

        }catch(IllegalArgumentException e1){
           throw e1;}
        catch(Exception e2){
           }
        boundsSatisfied=true;
        //if (ymin >
        //boundsSatisfied = checkBounds(
        }
  }
  /**
   * Create a spline with initial and terminal conditions, but no lower bound
   */
  public ConservativeSpline(double[] xval, double[] y, double[] pq,
                  double y0, double yn) throws IllegalArgumentException {
    this(xval, y, pq, y0, yn, DEFAULT_LBOUND);
  }

  /**
   * Public access of parameter A (B,C,P,...)
   * @return
   */
  public double[] getA(){
       return a;
       }
  public double[] getB(){
       return b;
       }
  public double[] getC(){
       return c;
       }

  public double[] getP(){
      return p;
      }

  public double[] getQ(){
      return q;
      }

  public void fitHistSpline(double[] y,
                  double y0, double yn)throws IllegalArgumentException, Exception{
    double h = 0;
    d = new double[n];
    e = new double[n];
    y1 = new double[n];
    double eps=EPS;

    e[0]=0;
    //check that abscissa values are ascending and form cumulative total
    for (int i=0, j=1; j<n; i++, j++){
      if (x[i] >= x[j]){
          throw new IllegalArgumentException("Abscissa (x) values must be in strictly ascending order.");
          }
      e[j]=e[i]+(x[j]-x[i])*y[i];
    }

    y1[0]=y0;
    y1[n-1]=yn;

    RationalSpline1(x,e,y0,yn);

    for (int i=0; i<(n-1); i++){
            h=1./(x[i+1]-x[i]);
            a[i]= h*(b[i]-a[i]);
            b[i]= h*c[i];
            c[i]=-h*d[i];
     }
}

private void RationalSpline1(double[] x, double[] y,
     double y0, double yn)throws Exception{
   int kp1,km1,k1;
   double pk2,pk21,pk22,pk,qk2,qk,p22,h,q22,g2,r2,z,p2,q2;
   double e1=EPS-1.;
   int n1=n-1;
   int n2=n-2;
   double r1 = 0;
   double qk1=0;
   double p21=0.;
   double g1=0;
   for(int k=0; k < n1; k++){
	  kp1=k+1;
	  km1=k-1;
	  pk=p[k];
	  qk=q[k];
	  pk2=pk*(pk+3.)+3.;
	  qk2=qk*(qk+3.)+3.;
	  p22=2.+pk;
	  q22=2.+qk;

          h= 1./(x[kp1] - x[k]);
	  a[k]=1./(p22*q22-1.);
	  g2=h*a[k];
	  r2=h*g2*(y[kp1]-y[k]);
	  if (k != 0){
            b[km1]=qk2*g2;
	    c[km1]=qk1*p21*g1 + pk2*q22*g2;
	    d[km1]=pk2*g2;
	    y1[km1]=r1*qk1*(1.+p21) + r2*pk2*(1.+q22);
	    if (k == 1)  y1[km1] = y1[km1] - qk1*g1*y0;
	    if (k == (n1-1)) y1[km1] =  y1[km1] - pk2*g2*yn;
          }
          p21=p22;
	  qk1=qk2;
	  g1=g2;
	  r1=r2;
    }
    tridiu(n-2,b,c,d,y1,EPS);

      for (int k=1; k<=n2; k++){
	  y1[n1-k]=y1[n2-k];
      }
      y1[0]=y0;
      y1[n-1]=yn;
      for (int k=0; k<n1; k++){
        k1=k+1;
        h=a[k]*(y[k1]-y[k]);
	z=a[k]*(x[k1]-x[k]);
	p2=2.+p[k];
	q2=2.+q[k];
	d[k]=-(1.+p2)*h+z*(p2*y1[k1]+y1[k]);
	c[k]=(1.+q2)*h-z*(y1[k1]+q2*y1[k]);
	b[k]=y[k1]-d[k];
	a[k]=y[k]-c[k];
      }
  }

  private void tridiu(int nn, double[] aa, double[] bb, double[] cc,
        double dd[], double eps)throws Exception{
	double h1,h2,h3,z;
	h1=0.;
	h2=0.;
	h3=0.;

	for( int k=0; k<nn; k++){
          z=bb[k]-h3*h1;
          if(Math.abs(z) < eps) throw new Exception("Singular problem");
	  h1=cc[k]/z;
	  cc[k]=h1;
	  h2=(dd[k]-h3*h2)/z;
	  bb[k]=h2;
	  h3=aa[k];
	}
        dd[nn-1]=bb[nn-1];
	for (int k=2 ; k<=nn; k++){
	  dd[nn-k]=bb[nn-k]-cc[nn-k]*dd[nn-k+1];
	}
  }
  /**
   * Calculates the value of the spline at a specified point x. Based on the
   * Fortran function rh2val from Spath
   * @param xnew abscissa (x value) at which spline is to be calculated.
   * @return the y (fit) value of the spline at xnew
   * @throws Exception
   */
  public double rh2val(double xnew)throws Exception{
    intone(xnew);
    double t= (xnew-x[indx]) / (x[indx+1] - x[indx]);
    double u=1.- t;
    double h1=p[indx]*t+1.;
    double h2=q[indx]*u+1.;
    return a[indx] + b[indx]*u*u*(2*p[indx]*u-3.*(1.+p[indx]))/(h1*h1) +
        c[indx]*t*t*(2.*q[indx]*t-3.*(1.+q[indx]))/(h2*h2);
    }

  private void intone(double v)throws Exception{
    int l=0;
    int k=0;
    if (indx >= (n-1)) indx=0;
    if (v < x[0] || v > x[n-1]) throw new Exception("New abscissa values out of bounds (extrapolation)");
    if (v >= x[indx]) {
       if (v < x[indx+1] ) return;
         l=(n-1);
       }else{
	 l=indx;
	 indx=0;
       }

       while (l > indx+1){
        k=(indx+l)/2;
        if (v < x[k]){
          l=k;
        }else{
          indx=k;
        }
       }
}


public static void main(String[] argv){

   double[] xx = {1,32,62,93,123,154,184,215,246,276};
   double[] yy = {100.,500.,700.,1040.,1040.,800.,190.,1300.,600.,400.};
   double y0=80;
   double yn=600;
   double[] pp = new double[10];
   for(int i = 0; i<10; i++){
     pp[i]=20.;
   }
   double[] xnew = new double[276];
   double[] ynew = new double[276];
   int[] index=new int[276];
   ConservativeSpline cs;
   try {
      cs = new ConservativeSpline(xx, yy, pp, y0,yn);
      for (int i=0; i<276; i++){
        xnew[i]=(double)(i+1);
        ynew[i]=cs.rh2val(xnew[i]);
      }
   }catch (Exception e){
      e.printStackTrace();
   }

   try{
     PrintWriter outfile = new PrintWriter(
       new BufferedWriter(
          new FileWriter("d:/delta/tide/models/ConserveSpline/out2.txt")));
      for(int i = 0; i<276; i++){
	  outfile.write(
          xnew[i]+"   "+ynew[i]+"\n");
      }
      outfile.close();
    }catch(java.io.IOException e){
       e.printStackTrace();
    }

    int ndeg=3;
    double[] coef={-2.,4.,-3.,1.};
    double[] realpt={0.,0.,0.};
    double[] imagpt={0.,0.,0.};
    RootFinding.DZPORC(ndeg,coef,realpt,imagpt);
    for (int j=0; j<3; j++){
      System.out.println(realpt[j]);
      System.out.println(imagpt[j]);
    }
  }

}