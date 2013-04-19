// <copyright file="CoinExtensionsTest.cs">Copyright ©  2013</copyright>
using System;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;

namespace VendingMachine.Data
{
    /// <summary>This class contains parameterized unit tests for CoinExtensions</summary>
    [PexClass(typeof(CoinExtensions))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(InvalidOperationException))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(ArgumentException), AcceptExceptionSubtypes = true)]
    [TestClass]
    public partial class CoinExtensionsTest
    {
        /// <summary>Test stub for ToValue(Coin)</summary>
        [PexMethod]
        public decimal ToValue(Coin coin)
        {
            decimal result = CoinExtensions.ToValue(coin);
            return result;
            // TODO: add assertions to method CoinExtensionsTest.ToValue(Coin)
        }
    }
}
