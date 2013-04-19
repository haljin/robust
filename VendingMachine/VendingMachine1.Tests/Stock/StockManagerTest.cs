// <copyright file="StockManagerTest.cs">Copyright ©  2013</copyright>
using System;
using System.Collections.Generic;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;
using VendingMachine.Stock;

namespace VendingMachine.Stock
{
    /// <summary>This class contains parameterized unit tests for StockManager</summary>
    [PexClass(typeof(StockManager))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(InvalidOperationException))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(ArgumentException), AcceptExceptionSubtypes = true)]
    [TestClass]
    public partial class StockManagerTest
    {
        /// <summary>Test stub for AddProduct(Product)</summary>
        [PexMethod]
        internal void AddProduct([PexAssumeUnderTest]StockManager target, Product product)
        {
            target.AddProduct(product);
            // TODO: add assertions to method StockManagerTest.AddProduct(StockManager, Product)
        }

        /// <summary>Test stub for CheckAvailability(Product)</summary>
        [PexMethod]
        internal bool CheckAvailability([PexAssumeUnderTest]StockManager target, Product product)
        {
            bool result = target.CheckAvailability(product);
            return result;
            // TODO: add assertions to method StockManagerTest.CheckAvailability(StockManager, Product)
        }

        /// <summary>Test stub for .ctor()</summary>
        [PexMethod]
        internal StockManager Constructor()
        {
            StockManager target = new StockManager();
            return target;
            // TODO: add assertions to method StockManagerTest.Constructor()
        }

        /// <summary>Test stub for EjectProduct(Product, LinkedList`1&lt;Product&gt;)</summary>
        [PexMethod]
        internal void EjectProduct(
            [PexAssumeUnderTest]StockManager target,
            Product product,
            LinkedList<Product> productCase
        )
        {
            target.EjectProduct(product, productCase);
            // TODO: add assertions to method StockManagerTest.EjectProduct(StockManager, Product, LinkedList`1<Product>)
        }

        /// <summary>Test stub for GetPrice(Product)</summary>
        [PexMethod]
        internal decimal GetPrice([PexAssumeUnderTest]StockManager target, Product product)
        {
            decimal result = target.GetPrice(product);
            return result;
            // TODO: add assertions to method StockManagerTest.GetPrice(StockManager, Product)
        }
    }
}
