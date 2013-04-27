using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Stock;
using VendingMachine.Data;
using System.Xml;
using System.Xml.Linq;

namespace VendingMachine.Stock
{

    public partial class StockManagerTest
    {
        // Scenario: Check a product that is in stock
        // Expected outcome: Returns true
        [TestMethod]
        public void CheckCorrect()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));
            bool result = stockMan.CheckAvailability(new Product("coke"));
            Assert.AreEqual(true, result);  
        }

        // Scenario: Check a product that is not in stock
        // Expected outcome: Returns false
        [TestMethod]
        public void CheckMissing()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));
            bool result = stockMan.CheckAvailability(new Product("nothing"));
            Assert.AreEqual(false, result);
        }

        // Scenario: Check a product that is null
        // Expected outcome: Precondition fails
        [TestMethod]
        public void CheckNull()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));
            try
            {
                bool result = stockMan.CheckAvailability(null);
                Assert.Fail();
            }
            catch (Exception)
            {
            }
            
        }

    }
}
