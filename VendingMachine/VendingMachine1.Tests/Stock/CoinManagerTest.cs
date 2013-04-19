// <copyright file="CoinManagerTest.cs">Copyright ©  2013</copyright>
using System;
using System.Collections.Generic;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;
using VendingMachine.Stock;

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
        [PexMethod]
        internal void AddCoin(
            [PexAssumeUnderTest]CoinManager target,
            Coin coin,
            int ammount
        )
        {
            target.AddCoin(coin, ammount);
            // TODO: add assertions to method CoinManagerTest.AddCoin(CoinManager, Coin, Int32)
        }

        /// <summary>Test stub for CheckChange(Decimal, Decimal)</summary>
        [PexMethod]
        internal bool CheckChange(
            [PexAssumeUnderTest]CoinManager target,
            decimal price,
            decimal inserted
        )
        {
            bool result = target.CheckChange(price, inserted);
            return result;
            // TODO: add assertions to method CoinManagerTest.CheckChange(CoinManager, Decimal, Decimal)
        }

        /// <summary>Test stub for .ctor()</summary>
        [PexMethod]
        internal CoinManager Constructor()
        {
            CoinManager target = new CoinManager();
            return target;
            // TODO: add assertions to method CoinManagerTest.Constructor()
        }

        /// <summary>Test stub for EjectCoin(Coin, Int32, LinkedList`1&lt;Coin&gt;)</summary>
        [PexMethod(MaxConditions = 1000)]
        internal void EjectCoin(
            [PexAssumeUnderTest]CoinManager target,
            Coin coin,
            int ammount,
            LinkedList<Coin> coinCase
        )
        {
            PexAssume.InRange((int)coin, (int)Coin.Ore50, (int)Coin.Kr20);
            target.EjectCoin(coin, ammount, coinCase);
            // TODO: add assertions to method CoinManagerTest.EjectCoin(CoinManager, Coin, Int32, LinkedList`1<Coin>)
        }

        /// <summary>Test stub for GiveChange(Decimal, Decimal, LinkedList`1&lt;Coin&gt;)</summary>
        [PexMethod]
        internal void GiveChange(
            [PexAssumeUnderTest]CoinManager target,
            decimal price,
            decimal inserted,
            LinkedList<Coin> coinCase
        )
        {
            target.GiveChange(price, inserted, coinCase);
            // TODO: add assertions to method CoinManagerTest.GiveChange(CoinManager, Decimal, Decimal, LinkedList`1<Coin>)
        }
    }
}
