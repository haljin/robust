// <copyright file="Form1Test.cs">Copyright ©  2013</copyright>
namespace VendingMachine.GUI
{
    /// <summary>This class contains parameterized unit tests for Form1</summary>
    [global::Microsoft.Pex.Framework.PexClass(typeof(global::VendingMachine.GUI.Form1))]
    [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.InvalidOperationException))]
    [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.ArgumentException), AcceptExceptionSubtypes = true)]
    [global::Microsoft.VisualStudio.TestTools.UnitTesting.TestClass]
    public partial class Form1Test
    {
        /// <summary>Test stub for .ctor()</summary>
        [global::Microsoft.Pex.Framework.PexMethod]
        public global::VendingMachine.GUI.Form1 Constructor()
        {
            global::VendingMachine.GUI.Form1 target = new global::VendingMachine.GUI.Form1()
              ;
            return target;
            // TODO: add assertions to method Form1Test.Constructor()
        }
    }
}
