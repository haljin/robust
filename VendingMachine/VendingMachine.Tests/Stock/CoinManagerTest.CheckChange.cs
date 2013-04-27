using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using System.Diagnostics.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace VendingMachine.Stock
{

    public partial class CoinManagerTest
    {
        // Scenario: Exact amount of money is input
        // Expected outcome: Returns true -> change is equal 0
        [TestMethod]
        public void CheckChangeExactAmountOfMoney()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>10</Ammount>
              </Coin>  
              </Root>";
            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));

            bool result = coinMan.CheckChange(10, 10);

            Assert.IsTrue(result);
        }

        // Scenario: Correct amount of change is returned
        // Expected outcome: Returns true
        [TestMethod]
        public void CheckChangeCorrect()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>10</Ammount>
              </Coin>  
              </Root>";
            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));

            bool result = coinMan.CheckChange(10, 11);

            Assert.IsTrue(result);
        }

        // Scenario: The machine does not have enough small coins to give change, although the total sum is enough
        // Expected outcome: Returns false
        [TestMethod]
        public void CheckChangeNotEnoughSmallCoins()
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
                <Ammount>0</Ammount>
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
                <Ammount>1</Ammount>
              </Coin>  
              </Root>";
            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));

            bool result = coinMan.CheckChange(1, 10);

            Assert.IsFalse(result);
        }

        // Scenario: The machine does not have enough money to return
        // Expected outcome: Return false
        [TestMethod]
        public void CheckChangeNotEnoughMoney()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>20</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>0</Ammount>
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

            bool result = coinMan.CheckChange(10, 21);

            Assert.IsFalse(result);
        }

        // Scenario: Incorrect parameter for inserted coins
        // Expected outcome: Function throws exception
        [TestMethod]
        public void CheckChangeIncorrectInserted()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>20</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>50</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>20</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>10</Ammount>
              </Coin>  
              </Root>";
            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));
            try
            {
                bool result = coinMan.CheckChange(10, -9);
                Assert.Fail();
            }
            catch (Exception)
            { }
            
            
        }

        // Scenario: Incorrect parameter for the price
        // Expected outcome: Function throws exceprion
        [TestMethod]
        public void CheckChangeIncorrectPrice()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>20</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>50</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>20</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>10</Ammount>
              </Coin>  
              </Root>";
            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));
            try
            {
                bool result = coinMan.CheckChange(-10, 9);
                Assert.Fail();
            }
            catch (Exception)
            { }


        }

        // Scenario: Parameter for price and inserted = 0
        // Expected outcome: Returns true
        [TestMethod]
        public void CheckChange0Inserted()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>20</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>50</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>10</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>20</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>10</Ammount>
              </Coin>  
              </Root>";
            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));
            
                bool result = coinMan.CheckChange(0, 0);
                Assert.IsTrue(result);
            


        }
    }
}
