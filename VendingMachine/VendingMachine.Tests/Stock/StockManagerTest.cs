using Microsoft.Pex.Framework;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using VendingMachine.Data;
// <copyright file="StockManagerTest.cs">Copyright ©  2013</copyright>
namespace VendingMachine.Stock
{
    /// <summary>This class contains parameterized unit tests for StockManager</summary>
    [global::Microsoft.Pex.Framework.PexClass(typeof(global::VendingMachine.Stock.StockManager))]
    [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.InvalidOperationException))]
    [global::Microsoft.Pex.Framework.Validation.PexAllowedExceptionFromTypeUnderTest(typeof(global::System.ArgumentException), AcceptExceptionSubtypes = true)]
    [global::Microsoft.VisualStudio.TestTools.UnitTesting.TestClass]
    public partial class StockManagerTest
    {
        /// <summary>Test stub for AddProduct(Product)</summary>
        [global::Microsoft.Pex.Framework.PexMethod]
        internal void AddProduct([global::Microsoft.Pex.Framework.PexAssumeUnderTest]global::VendingMachine.Stock.StockManager target, global::VendingMachine.Data.Product product)
        {
            target.AddProduct(product);
            
        }

    }
}
