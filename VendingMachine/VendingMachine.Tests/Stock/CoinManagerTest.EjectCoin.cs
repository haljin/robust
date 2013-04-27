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
    {
        //Pex generated
        [TestMethod]
        public void EjectCoinThrowsContractException868()
        {
            try
            {
                CoinManager coinManager;
                coinManager = new CoinManager();
                this.EjectCoin(coinManager, Coin.Ore50, 1, (LinkedList<Coin>)null);
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
        public void EjectCoin502()
        {
            CoinManager coinManager;
            LinkedList<Coin> linkedList;
            LinkedList<Coin> linkedList1;
            coinManager = new CoinManager();
            Coin[] @is = new Coin[0];
            linkedList = new LinkedList<Coin>((IEnumerable<Coin>)@is);
            linkedList1 = new LinkedList<Coin>((IEnumerable<Coin>)linkedList);
            this.EjectCoin(coinManager, Coin.Ore50, 1, linkedList1);
            Assert.IsNotNull((object)coinManager);
        }

        //Pex generated
        [TestMethod]
        public void EjectCoin257()
        {
            CoinManager coinManager;
            LinkedList<Coin> linkedList;
            LinkedList<Coin> linkedList1;
            coinManager = new CoinManager();
            Coin[] @is = new Coin[0];
            linkedList = new LinkedList<Coin>((IEnumerable<Coin>)@is);
            linkedList1 = new LinkedList<Coin>((IEnumerable<Coin>)linkedList);
            this.EjectCoin(coinManager, Coin.Kr1, 1, linkedList1);
            Assert.IsNotNull((object)coinManager);
        }

        // Scenario: Machine does not have enough coins to give back
        // Expected outcome: Exception should be thrown
        [TestMethod]
        public void EjectCoinNotEnoughCoins()
        {

            string doc =
           @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>1</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";
            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));
            LinkedList<Coin> linkedList = new LinkedList<Coin>();

            try
            {
                coinMan.EjectCoin(Coin.Kr1, 2, linkedList);
                Assert.Fail();
            }
            catch (Exception)
            { }

        }
        // Scenario: Adding more coins to not empty case
        // Expected outcome: Exception should be thrown
        [TestMethod]
        public void EjectCoinCaseNotEmpty()
        {

            string doc =
           @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";
            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));



            LinkedList<Coin> linkedList = new LinkedList<Coin>();
            linkedList.AddLast(Coin.Kr1);
            linkedList.AddLast(Coin.Kr1);

            coinMan.EjectCoin(Coin.Kr1, 2, linkedList);

            Assert.IsNotNull(coinMan);

        }
    }
}
