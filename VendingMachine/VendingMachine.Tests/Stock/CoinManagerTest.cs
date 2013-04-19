using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using VendingMachine.Data;
// <copyright file="CoinManagerTest.cs">Copyright ©  2013</copyright>

namespace VendingMachine.Stock
{
    /// <summary>This class contains parameterized unit tests for CoinManager</summary>
    [PexClass(typeof(global::VendingMachine.Stock.CoinManager))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(global::System.InvalidOperationException))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(global::System.ArgumentException), AcceptExceptionSubtypes = true)]
    [global::Microsoft.VisualStudio.TestTools.UnitTesting.TestClass]
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

            PexAssume.IsTrue(5 >= (int)coin && 0 <= (int) coin, "proper coin");
            target.AddCoin(coin, ammount);
        }

        /// <summary>Test stub for EjectCoin(Coin, Int32, LinkedList`1&lt;Coin&gt;)</summary>
        [PexMethod(MaxConditions = 3000)]
        internal void EjectCoin(
            [PexAssumeUnderTest]CoinManager target,
            Coin coin,
            int ammount,
            LinkedList<global::VendingMachine.Data.Coin> coinCase
        )
        {
            PexAssume.IsTrue(5 >= (int)coin && 0 <= (int)coin, "proper coin");
            PexAssume.InRange(ammount, 1, 10);
            target.EjectCoin(coin, ammount, coinCase);
        }

    }
}
