using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using VendingMachine.Data;

namespace VendingMachine.GUI
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();

            Stock.StockManager man = new Stock.StockManager();
            Stock.CoinManager cman = new Stock.CoinManager();


            LinkedList<Coin> coinCase = new LinkedList<Coin>();

            cman.AddCoin(Coin.Kr20, 10);

            bool flag = cman.CheckChange(10, 20);

            cman.GiveChange(10, 37.5m, coinCase);

            flag = cman.CheckChange(1, 1000000);
            
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Product test = new Product("Lol", 10, 20);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Product test = new Product("Lol", 10, -10);
        }
    }
}
