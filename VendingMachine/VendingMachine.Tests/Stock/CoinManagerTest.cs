using System;
// <copyright file="CoinManagerTest.cs">Copyright ©  2013</copyright>
using System.Collections.Generic;
using System.Xml.Linq;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;


namespace VendingMachine.Stock
{
    /// <summary>This class contains parameterized unit tests for CoinManager</summary>
    [PexClass(typeof(CoinManager))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(InvalidOperationException))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(ArgumentException), AcceptExceptionSubtypes = true)]
    [TestClass]
    public partial class CoinManagerTest
    {
        /// <summary>Test stub for AddCoin(Coin, Int32)</summary>
        [PexMethod(MaxConditions = 2000)]
        internal void AddCoin(
            [PexAssumeUnderTest]CoinManager target,
            Coin coin,
            int ammount
        )
        {

            PexAssume.IsTrue(5 >= (int)coin && 0 <= (int)coin, "proper coin");
            target.AddCoin(coin, ammount);
        }

        /// <summary>Test stub for EjectCoin(Coin, Int32, LinkedList`1&lt;Coin&gt;)</summary>
        [PexMethod(MaxConditions = 3000)]
        internal void EjectCoin(
            [PexAssumeUnderTest]CoinManager target,
            Coin coin,
            int ammount,
            LinkedList<Coin> coinCase
        )
        {
            PexAssume.IsTrue(5 >= (int)coin && 0 <= (int)coin, "proper coin");
            PexAssume.InRange(ammount, 1, 10);
            target.EjectCoin(coin, ammount, coinCase);
        }

        // Scenario: Database XML file is incorrect
        // Expected outcome: Constructor throws exception
        [TestMethod]
        public void ConstructorTest()
        {
            string doc =
                @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
            </Root>";
            try
            {
                CoinManager coinMan = new CoinManager(XDocument.Parse(doc));
                Assert.Fail();
            }
            catch (Exception)
            { }

        }

        // Scenario: Database is null
        // Expected outcome: Constructor throws exception
        public void ConstructorTest2()
        {
            string doc = null;
            try
            {
                CoinManager coinMan = new CoinManager(XDocument.Parse(doc));
                Assert.Fail();
            }
            catch (Exception)
            { }

        }


    }
}
