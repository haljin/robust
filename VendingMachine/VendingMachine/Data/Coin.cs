using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text;

namespace VendingMachine.Data
{
    public enum Coin
    {
        Ore, Kr1, Kr2, Kr5, Kr10, Kr20
    }

    public static class CoinExtensions
    {
        public static decimal ToValue(this Coin coin)
        {
            Contract.Ensures(Contract.Result<decimal>() > 0);
            switch (coin)
            {
                case Coin.Ore:
                    return 0.5m;
                case Coin.Kr1:
                    return 1;
                case Coin.Kr2:
                    return 2;
                case Coin.Kr5:
                    return 5;
                case Coin.Kr10:
                    return 10;
                case Coin.Kr20:
                    return 20;
                default:
                    return 0;
            }
            
            
        }
        
    }
}
