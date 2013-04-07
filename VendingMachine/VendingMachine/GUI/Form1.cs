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

namespace VendingMachine.GUI
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();

            VendMachine machine = new VendMachine();

            TransactionResult res = machine.SelectProduct(new Product("Cola"));

            res = machine.InsertCoin(Coin.Kr20);
            res = machine.InsertCoin(Coin.Kr20);

            res = machine.Finalize();

            res = machine.SelectProduct(new Product("Dupa"));
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
