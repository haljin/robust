// <copyright file="CoinExtensionsTest.cs">Copyright ©  2013</copyright>
namespace VendingMachine.Data
{
    /// <summary>This class contains parameterized unit tests for CoinExtensions</summary>
    [global::Microsoft.Pex.Framework.PexClass(typeof(global::VendingMachine.Data.CoinExtensions))]
    [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.InvalidOperationException))]
    [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.ArgumentException), AcceptExceptionSubtypes = true)]
    [global::Microsoft.VisualStudio.TestTools.UnitTesting.TestClass]
    public partial class CoinExtensionsTest
    {
        /// <summary>Test stub for ToValue(Coin)</summary>
        [global::Microsoft.Pex.Framework.PexMethod]
        public decimal ToValue(global::VendingMachine.Data.Coin coin)
        {
            decimal result = global::VendingMachine.Data.CoinExtensions.ToValue(coin);
            return result;
            // TODO: add assertions to method CoinExtensionsTest.ToValue(Coin)
        }
    }
}
