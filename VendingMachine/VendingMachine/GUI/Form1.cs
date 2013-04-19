using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using VendingMachine.Data;
using VendingMachine.Machine;
using VendingMachine.Stock;

namespace VendingMachine.GUI
{
    public partial class Form1 : Form
    {
        private VendMachine machine;

        public Form1()
        {
            InitializeComponent();
            machine = new VendMachine();
            prepareOutOfStockLights();

            CoinManager coins = new CoinManager();
            coins.EjectCoin(Coin.Ore50, 1, new LinkedList<Coin>());
        }

        private void purchaseB_Click(object sender, EventArgs e)
        {
            if (machine.SelectedProduct != null) // we need to choose the product in order to purchase it
            {
                insertCoinB.Enabled = true;
                cancelB.Enabled = true;
                switch (machine.SelectedProduct.Name)
                {
                    case "coke":
                        faxeB.Enabled = false;
                        hariboB.Enabled = false;
                        daimB.Enabled = false;
                        break;
                    case "faxe":
                        cokeB.Enabled = false;
                        hariboB.Enabled = false;
                        daimB.Enabled = false;
                        break;
                    case "haribo":
                        cokeB.Enabled = false;
                        faxeB.Enabled = false;
                        daimB.Enabled = false;
                        break;
                    case "daim":
                        cokeB.Enabled = false;
                        faxeB.Enabled = false;
                        hariboB.Enabled = false;
                        break;
                }
            }
        }

        private void cokeB_Click(object sender, EventArgs e)
        {
            Product prod = new Product("coke");
            machine.SelectProduct(prod);
            // update disp
            decimal valueToBeInserted = machine.SelectedProduct.Price - machine.InsertedValue;
            dispL.Text = prepareDisp(valueToBeInserted);
            purchaseB.Enabled = true;
        }

        private void faxeB_Click(object sender, EventArgs e)
        {
            Product prod = new Product("faxe");
            machine.SelectProduct(prod);
            // update disp
            decimal valueToBeInserted = machine.SelectedProduct.Price - machine.InsertedValue;
            dispL.Text = prepareDisp(valueToBeInserted);
            purchaseB.Enabled = true;
        }

        private void hariboB_Click(object sender, EventArgs e)
        {
            Product prod = new Product("haribo");
            machine.SelectProduct(prod);
            // update disp
            decimal valueToBeInserted = machine.SelectedProduct.Price - machine.InsertedValue;
            dispL.Text = prepareDisp(valueToBeInserted);
            purchaseB.Enabled = true;
        }

        private void daimB_Click(object sender, EventArgs e)
        {
            Product prod = new Product("daim");
            machine.SelectProduct(prod);
            // update disp
            decimal valueToBeInserted = machine.SelectedProduct.Price - machine.InsertedValue;
            dispL.Text = prepareDisp(valueToBeInserted);
            purchaseB.Enabled = true;
        }

        private void insertCoinB_Click(object sender, EventArgs e)
        {
            if (coinsCB.SelectedItem != null)
            {
                TransactionResult tr = new TransactionResult();
                switch (coinsCB.SelectedItem.ToString())
                {
                    case "Ore50":
                        tr = machine.InsertCoin(Coin.Ore50);
                        break;
                    case "Kr1":
                        tr = machine.InsertCoin(Coin.Kr1);
                        break;
                    case "Kr2":
                        tr = machine.InsertCoin(Coin.Kr2);
                        break;
                    case "Kr5":
                        tr = machine.InsertCoin(Coin.Kr5);
                        break;
                    case "Kr10":
                        tr = machine.InsertCoin(Coin.Kr10);
                        break;
                    case "Kr20":
                        tr = machine.InsertCoin(Coin.Kr20);
                        break;
                }
                if (machine.Finalize() == TransactionResult.Success)
                {
                    MessageBox.Show("RELEASED PRODUCTS:\n" + machine.ReleasedProducts() + "\n\nCHANGE:\n" + machine.ReleasedCoins() );
                    // clicking on OK of MessageBox is treated as the product and change was taken off from case
                    machine.EmptyCases();

                    insertCoinB.Enabled = false;
                    purchaseB.Enabled = false;
                    dispL.Text = "-----";
                    enableAllProd();
                    prepareOutOfStockLights();

                }
                else // update display
                {
                    decimal val = (machine.SelectedProduct.Price - machine.InsertedValue);
                    dispL.Text = prepareDisp(val);
                }
            }
            // nothing entered
        }

        private void cancelB_Click(object sender, EventArgs e)
        {
            machine.Cancel();
            MessageBox.Show("TRANSACTION CANCELLED.\n\nCHANGE:\n" + machine.ReleasedCoins() );
            // clicking on OK of MessageBox is treated as the product and change was taken off from case
            machine.EmptyCases();
            
            // prepare for next transaction
            dispL.Text = "-----";
            enableAllProd();
            prepareOutOfStockLights();
        }

        private string prepareDisp(decimal value) 
        {
            int digits = value.ToString().Length;
            string retStr = "";
            for (int i = 0; i < 5 - digits; i++)
            {
                retStr += "-";
            }
            return retStr + value.ToString();
        }

        private void enableAllProd()
        {
            cokeB.Enabled = true;
            faxeB.Enabled = true;
            hariboB.Enabled = true;
            daimB.Enabled = true;
        }

        private void prepareOutOfStockLights()
        {
            if (machine.CheckProduct(new Product("coke")) == TransactionResult.OutOfStock)
            {
                cokeATB.BackColor = Color.Red;
                cokeB.Enabled = false;
            }
            if (machine.CheckProduct(new Product("faxe")) == TransactionResult.OutOfStock)
            {
                faxeATB.BackColor = Color.Red;
                faxeB.Enabled = false;
            }
            if (machine.CheckProduct(new Product("haribo")) == TransactionResult.OutOfStock)
            {
                hariboATB.BackColor = Color.Red;
                hariboB.Enabled = false;
            }
            if (machine.CheckProduct(new Product("daim")) == TransactionResult.OutOfStock)
            {
                daimATB.BackColor = Color.Red;
                daimB.Enabled = false;
            }
        }

    }
}
