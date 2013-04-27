using System;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
// <copyright file="CoinManagerTest.cs">Copyright ©  2013</copyright>
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;
// <copyright file="StockManagerTest.cs">Copyright ©  2013</copyright>
namespace VendingMachine.Stock
{
    /// <summary>This class contains parameterized unit tests for StockManager</summary>
    [PexClass(typeof(global::VendingMachine.Stock.StockManager))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(InvalidOperationException))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(ArgumentException), AcceptExceptionSubtypes = true)]
    [TestClass]
    public partial class StockManagerTest
    {
        /// <summary>Test stub for AddProduct(Product)</summary>
        [PexMethod(MaxConditions = 2000)]
        internal void AddProduct([PexAssumeUnderTest]StockManager target, Product product)
        {
            target.AddProduct(product);
            
        }

        [PexFactoryMethod(typeof(Product))]
        public static Product Create(string name, int amm, decimal price)
        {
            Product prod = new Product(name, amm, price);

            return prod;
        }

    }

    
}
