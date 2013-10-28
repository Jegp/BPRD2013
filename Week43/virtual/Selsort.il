    // method line 2
    .method public static hidebysig 
           default void SelectionSort (int32[] arr)  cil managed 
    {
        // Method begins at RVA 0x20c0
	// Code size 72 (0x48)
	.maxstack 4
	.locals init (
		int32	V_0,
		int32	V_1,
		int32	V_2,
		int32	V_3)
	IL_0000:  ldc.i4.0 
	IL_0001:  stloc.0 
	IL_0002:  br IL_003e

	IL_0007:  ldloc.0 
	IL_0008:  stloc.1 
	IL_0009:  ldloc.0 
	IL_000a:  ldc.i4.1 
	IL_000b:  add 
	IL_000c:  stloc.2 
	IL_000d:  br IL_0023

	IL_0012:  ldarg.0 
	IL_0013:  ldloc.2 
	IL_0014:  ldelem.i4 
	IL_0015:  ldarg.0 
	IL_0016:  ldloc.1 
	IL_0017:  ldelem.i4 
	IL_0018:  bge IL_001f

	IL_001d:  ldloc.2 
	IL_001e:  stloc.1 
	IL_001f:  ldloc.2 
	IL_0020:  ldc.i4.1 
	IL_0021:  add 
	IL_0022:  stloc.2 
	IL_0023:  ldloc.2 
	IL_0024:  ldarg.0 
	IL_0025:  ldlen 
	IL_0026:  conv.i4 
	IL_0027:  blt IL_0012

	IL_002c:  ldarg.0 
	IL_002d:  ldloc.0 
	IL_002e:  ldelem.i4 
	IL_002f:  stloc.3 
	IL_0030:  ldarg.0 
	IL_0031:  ldloc.0 
	IL_0032:  ldarg.0 
	IL_0033:  ldloc.1 
	IL_0034:  ldelem.i4 
	IL_0035:  stelem.i4 
	IL_0036:  ldarg.0 
	IL_0037:  ldloc.1 
	IL_0038:  ldloc.3 
	IL_0039:  stelem.i4 
	IL_003a:  ldloc.0 
	IL_003b:  ldc.i4.1 
	IL_003c:  add 
	IL_003d:  stloc.0 
	IL_003e:  ldloc.0 
	IL_003f:  ldarg.0 
	IL_0040:  ldlen 
	IL_0041:  conv.i4 
	IL_0042:  blt IL_0007

	IL_0047:  ret 
    } // end of method Selsort::SelectionSort

