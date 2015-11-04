import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.VolatileCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

public class IDDL
{
   public static MethodHandle onMode;
   public static MethodHandle offMode;
   static public VolatileCallSite site = null;

   static public long f_calls = 0;
   static public long g_calls = 0;

   // --------------------------------------------------------------------------------
   // Our two function calls to toggle between:

   public static void f()
   {
      f_calls ++;
   }

   public static void g() {
      g_calls ++;
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
