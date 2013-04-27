using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using System.Diagnostics.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Generated;
using Microsoft.Pex.Framework.Validation;
using VendingMachine.Data;
namespace VendingMachine.Stock
{

    public partial class CoinManagerTest
    {   //Pex generated
        [TestMethod]
        public void AddCoinThrowsContractException830()
        {
            try
            {
                CoinManager coinManager;
                coinManager = new CoinManager();
                this.AddCoin(coinManager, Coin.Ore50, 0);
                throw
                  new AssertFailedException("expected an exception of type ContractException");
            }
            catch (Exception ex)
            {
                if (!PexContract.IsContractException(ex))
                    throw ex;
            }
        }

        //Pex generated
        [TestMethod]
        public void AddCoin911()
        {
            CoinManager coinManager;
            coinManager = new CoinManager();
            this.AddCoin(coinManager, Coin.Ore50, 1);
            Assert.IsNotNull((object)coinManager);
        }

        //Pex generated
        [TestMethod]
        public void AddCoin761()
        {
            CoinManager coinManager;
            coinManager = new CoinManager();
            this.AddCoin(coinManager, Coin.Kr2, 2);
            Assert.IsNotNull((object)coinManager);
        }

        //Pex generated
        [TestMethod]
        public void AddCoin494()
        {
            CoinManager coinManager;
            coinManager = new CoinManager();
            this.AddCoin(coinManager, Coin.Kr1, 1);
            Assert.IsNotNull((object)coinManager);
        }
    }
}
