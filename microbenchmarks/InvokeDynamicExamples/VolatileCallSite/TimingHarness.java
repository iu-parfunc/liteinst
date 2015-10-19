
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.VolatileCallSite;

public class TimingHarness
{
   private static int trials = 10000;
   private static int trials_kept = 1000;
   private static long[] times = null;

   private static void summarize(String msg)
   {
       long mn = times[0];
       long mx = times[0];
       long sum = 0;
       for (long x : times) {
           mn = Math.min(mn,x);
           mx = Math.max(mx,x);
           sum += x;
       }
       System.out.printf("%s, min %d, max %d, avg %d\n", msg, mn,mx, sum / trials_kept);
       System.out.printf("  count of active_calls: %d\n", IDDL.active_calls);
   }

   public static void main(String[] args) throws Throwable
   {
      long t1=0, t2=0, t3=0, t4=0;
      t1 = System.nanoTime ();
      t2 = System.nanoTime ();
      System.out.printf("Smallest time for System.nanotime, at first: %d\n", t2-t1);
      for(int i=0; i < 10000; i++) {
          t1 = System.nanoTime ();
          t2 = System.nanoTime ();
      }
      System.out.printf("And after some warmup: %d\n", t2-t1);

      //      System.out.printf("Calling IDD.main once...\n");
      //      IDD.main(null);

      // MethodHandles.Lookup lookup = MethodHandles.lookup();
      // MethodHandle mh = lookup.findStatic(InvokeDynamicTest2.class, "noop",
      //                                      MethodType.methodType(int.class, int.class));

      times = new long[trials_kept];
      int result = 0;

      for (int i=0; i<trials; i++) {
          t1 = System.nanoTime ();
          IDDL.noop();
          t2 = System.nanoTime ();
          times[i % trials_kept] = t2-t1;
      }
      summarize("Time for direct call to noop()");

      for (int i=0; i<trials; i++) {
          t1 = System.nanoTime ();
          IDDL.active_call();
          t2 = System.nanoTime ();
          times[i % trials_kept] = t2-t1;
      }
      summarize("Time for direct call to active_call()");

      for (int i=0; i<trials; i++) {
          t1 = System.nanoTime ();
          IDD.main(null);
          t2 = System.nanoTime ();
          times[i % trials_kept] = t2-t1;
      }
      summarize("Time for direct call to IDD.main() and then thru VolatileCallSite");

      // --------------------------------------------------------------------------------
      // Next for toggling:

      for (int i=0; i<trials; i++) {
          t1 = System.nanoTime ();
          IDDL.site.setTarget(IDDL.onMode);
          t2 = System.nanoTime ();
          IDD.main(null);

          t3 = System.nanoTime ();
          IDDL.site.setTarget(IDDL.offMode);
          t4 = System.nanoTime ();
          IDD.main(null);

          times[i % trials_kept] = t2-t1;
          i++;
          times[i % trials_kept] = t4-t3;
      }
      summarize("Time for one VolatileCallSite toggle");

   }
}
