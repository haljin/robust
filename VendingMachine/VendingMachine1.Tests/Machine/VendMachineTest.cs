// <copyright file="VendMachineTest.cs">Copyright ©  2013</copyright>
using System;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;
using VendingMachine.Machine;

namespace VendingMachine.Machine
{
    /// <summary>This class contains parameterized unit tests for VendMachine</summary>
    [PexClass(typeof(VendMachine))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(InvalidOperationException))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(ArgumentException), AcceptExceptionSubtypes = true)]
    [TestClass]
    public partial class VendMachineTest
    {
        /// <summary>Test stub for Cancel()</summary>
        [PexMethod]
        public void Cancel([PexAssumeUnderTest]VendMachine target)
        {
            target.Cancel();
            // TODO: add assertions to method VendMachineTest.Cancel(VendMachine)
        }

        /// <summary>Test stub for CheckProduct(Product)</summary>
        [PexMethod]
        public TransactionResult CheckProduct([PexAssumeUnderTest]VendMachine target, Product product)
        {
            TransactionResult result = target.CheckProduct(product);
            return result;
            // TODO: add assertions to method VendMachineTest.CheckProduct(VendMachine, Product)
        }

        /// <summary>Test stub for .ctor()</summary>
        [PexMethod]
        public VendMachine Constructor()
        {
            VendMachine target = new VendMachine();
            return target;
            // TODO: add assertions to method VendMachineTest.Constructor()
        }

        /// <summary>Test stub for EmptyCases()</summary>
        [PexMethod]
        public void EmptyCases([PexAssumeUnderTest]VendMachine target)
        {
            target.EmptyCases();
            // TODO: add assertions to method VendMachineTest.EmptyCases(VendMachine)
        }

        /// <summary>Test stub for Finalize()</summary>
        [PexMethod]
        public TransactionResult Finalize01([PexAssumeUnderTest]VendMachine target)
        {
            TransactionResult result = target.Finalize();
            return result;
            // TODO: add assertions to method VendMachineTest.Finalize01(VendMachine)
        }

        /// <summary>Test stub for InsertCoin(Coin)</summary>
        [PexMethod]
        public TransactionResult InsertCoin([PexAssumeUnderTest]VendMachine target, Coin coin)
        {
            TransactionResult result = target.InsertCoin(coin);
            return result;
            // TODO: add assertions to method VendMachineTest.InsertCoin(VendMachine, Coin)
        }

        /// <summary>Test stub for ReleasedCoins()</summary>
        [PexMethod]
        public string ReleasedCoins([PexAssumeUnderTest]VendMachine target)
        {
            string result = target.ReleasedCoins();
            return result;
            // TODO: add assertions to method VendMachineTest.ReleasedCoins(VendMachine)
        }

        /// <summary>Test stub for ReleasedProducts()</summary>
        [PexMethod]
        public string ReleasedProducts([PexAssumeUnderTest]VendMachine target)
        {
            string result = target.ReleasedProducts();
            return result;
            // TODO: add assertions to method VendMachineTest.ReleasedProducts(VendMachine)
        }

        /// <summary>Test stub for SelectProduct(Product)</summary>
        [PexMethod]
        public TransactionResult SelectProduct([PexAssumeUnderTest]VendMachine target, Product product)
        {
            TransactionResult result = target.SelectProduct(product);
            return result;
            // TODO: add assertions to method VendMachineTest.SelectProduct(VendMachine, Product)
        }
    }
}
