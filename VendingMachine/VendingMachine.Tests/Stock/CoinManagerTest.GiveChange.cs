using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using System.Diagnostics.Contracts;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;

namespace VendingMachine.Stock
{

    public partial class CoinManagerTest
    {
        // Scenario: Normal change operation
        // Expected outcome: Executes correctly
        [TestMethod]
        public void GetChangeCorrect()
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
            LinkedList<Coin> list = new LinkedList<Coin>();

            coinMan.GiveChange(10, 20,list);

            decimal sum=0;
            foreach (Coin elem in list)
            { 
               sum += elem.ToValue();
            }
            Assert.AreEqual(10, sum);
        }

        // Scenario: Machine has no change
        // Expected outcome: Returns 0 kr
         [TestMethod]
        public void GetChangeNoChange()
        {string doc =
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
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";

            CoinManager coinMan = new CoinManager(XDocument.Parse(doc));
            LinkedList<Coin> list = new LinkedList<Coin>();

            coinMan.GiveChange(20, 25, list);
            

            decimal sum = 0;
            foreach (Coin elem in list)
            {
                sum += elem.ToValue();
            }
            Assert.AreEqual(0, sum);
        }


         // Scenario: Machine does not have enough coins to return
         // Expected outcome: Returns as many small coins as possible
         [TestMethod]
         public void GetChangeNotEnoughSmallCoins()
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
             LinkedList<Coin> list = new LinkedList<Coin>();

             coinMan.GiveChange(5, 19, list);


             decimal sum = 0;
             foreach (Coin elem in list)
             {
                 sum += elem.ToValue();
             }
             Assert.AreEqual(5, sum);
         }

         // Scenario: Inserted amound of money is not enough
         // Expected outcome: Exception should be thrown (Precondition fails)
         [TestMethod]
         public void GetChangeNotEnoughMoney()
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
             LinkedList<Coin> list = new LinkedList<Coin>();          

             try
             {
                 coinMan.GiveChange(5, 4, list);
                 Assert.Fail();
             }
             catch (Exception)
             { }

             
         }

         // Scenario: Coin case is null
         // Expected outcome: Exception should be thrown (Precondition fails)
         [TestMethod]
         public void GetChangeCaseNull()
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
             LinkedList<Coin> list = null;



             try
             {
                 coinMan.GiveChange(5, 5, list);
                 Assert.Fail();
             }
             catch (Exception)
             { }


         }

         // Scenario: Invalid price parameter
         // Expected outcome: Exception should be thrown (Precondition fails)
         [TestMethod]
         public void GetChangeInvalidPrice()
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
             LinkedList<Coin> list = new LinkedList<Coin>();

             try
             {
                 coinMan.GiveChange(-5, 5, list);
                 Assert.Fail();
             }
             catch (Exception)
             { }


         }

         // Scenario: Invalid inserted coins parameter
         // Expected outcome: Exception should be thrown (Precondition fails)
         [TestMethod]
         public void GetChangeInvalidInserted()
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
             LinkedList<Coin> list = new LinkedList<Coin>();



             try
             {
                 coinMan.GiveChange(5, -9, list);
                 Assert.Fail();
             }
             catch (Exception)
             { }


         }
    }
}
