import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.VolatileCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.concurrent.atomic.AtomicInteger;

public class IDDL
{
   // How many words in a cache line:
   public static final int PAD = 8;
   public static final int max_threads = 256;

   public static MethodHandle onMode;
   public static MethodHandle offMode;
   static public VolatileCallSite site = null;

   static public long f_calls[] = new long[max_threads * PAD];
   static public long g_calls[] = new long[max_threads * PAD];

   private static final AtomicInteger nextId = new AtomicInteger(0);

   static public ThreadLocal<Integer> threadId = new ThreadLocal<Integer>() {
           @Override protected Integer initialValue() {
               return nextId.getAndIncrement();
           }
       };

   // --------------------------------------------------------------------------------
   // Our two function calls to toggle between:

   public static void f()
   {
      int id = threadId.get();
      f_calls[id * PAD] ++;
   }

   public static void g() {
      int id = threadId.get();
      g_calls[id * PAD] ++;
   }

   // --------------------------------------------------------------------------------

   public static CallSite bootstrapDynamic(MethodHandles.Lookup caller,
                                           String name,
                                           MethodType type)
      throws IllegalAccessException, NoSuchMethodException
   {
      MethodHandles.Lookup lookup = MethodHandles.lookup();
      Class thisClass = lookup.lookupClass();
      onMode = lookup.findStatic(thisClass, "f",
                             MethodType.methodType(void.class));
      if (!type.equals(onMode.type()))
         onMode = onMode.asType(type);

      offMode = lookup.findStatic(thisClass, "g",
                             MethodType.methodType(void.class));
      if (!type.equals(offMode.type()))
         offMode = offMode.asType(type);

      site = new VolatileCallSite(onMode);

      System.out.println("BOOTSTRAPPING callsite " + site.toString() );

      return site;
   }

    public static void changeSite()
    {
        System.out.printf("Mutating call site...\n");
        //   site.
    }


}
