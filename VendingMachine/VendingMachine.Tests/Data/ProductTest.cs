// <copyright file="ProductTest.cs">Copyright ©  2013</copyright>
namespace VendingMachine.Data
{
    /// <summary>This class contains parameterized unit tests for Product</summary>
    [global::Microsoft.Pex.Framework.PexClass(typeof(global::VendingMachine.Data.Product))]
    [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.InvalidOperationException))]
    [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.ArgumentException), AcceptExceptionSubtypes = true)]
    [global::Microsoft.VisualStudio.TestTools.UnitTesting.TestClass]
    public partial class ProductTest
    {
        /// <summary>Test stub for Equals(Object)</summary>
        [global::Microsoft.Pex.Framework.PexMethod]
        [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.NullReferenceException))]
        public bool Equals01([global::Microsoft.Pex.Framework.PexAssumeUnderTest]global::VendingMachine.Data.Product target, object obj)
        {
            bool result = target.Equals(obj);
            return result;
            // TODO: add assertions to method ProductTest.Equals01(Product, Object)
        }

    }
}
