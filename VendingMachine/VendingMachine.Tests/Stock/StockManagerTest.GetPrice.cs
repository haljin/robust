using System;
using System.Collections.Generic;
using System.Xml.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;


namespace VendingMachine.Stock
{
	public partial class StockManagerTest
	{
		[TestMethod]
        // Scenario: Get price from existing Item
        // Expected outcome: Price returned
        public void GetPriceCorrect()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              <Product>
                <Name>faxe</Name>
                <Price>12</Price>
                <Ammount>10</Ammount>
              </Product>
              <Product>
                <Name>daim</Name>
                <Price>7</Price>
                <Ammount>1</Ammount>
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));
           

            stockMan.GetPrice(new Product("faxe"));

        }
        // Scenario: Get price from not existing item 
        // Expected outcome: Exception should be thrown
        [TestMethod]
        public void GetPriceNonExistent()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              <Product>
                <Name>faxe</Name>
                <Price>12</Price>
                <Ammount>10</Ammount>
              </Product>
              <Product>
                <Name>daim</Name>
                <Price>7</Price>
                <Ammount>1</Ammount>
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));

            try
            {
                stockMan.GetPrice(new Product("lol"));
                Assert.Fail();
            }
            catch (Exception)
            { }


        }
    
    }
}
