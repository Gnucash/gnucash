<?xml version="1.0"?>
<gnc>
  <version>1</version>
  <ledger-data>
    <commodity>
      <restore>
        <space>NYSE</space>
        <id>O-Sicav Plus</id>
        <name>O-Sicav Plus</name>
        <fraction>100000</fraction>
      </restore>
    </commodity>
    <account>
      <restore>
        <name>Livret bleu</name>
        <guid>3bcb236af583d5bd7b6e0153fe97e7e3</guid>
        <type>BANK</type>
        <currency>
          <space>ISO4217</space>
          <id>FRF</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>Money95mfunds fr</name>
        <guid>89e091b3489a50957b400a868f101d59</guid>
        <type>STOCK</type>
        <currency>
          <space>ISO4217</space>
          <id>FRF</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>O-Sicav Plus</name>
        <guid>e0e954412d6b31dfa1fcbb9937a125b0</guid>
        <type>STOCK</type>
        <currency>
          <space>ISO4217</space>
          <id>FRF</id>
        </currency>
        <security>
          <space>NYSE</space>
          <id>O-Sicav Plus</id>
        </security>
        <parent>
          <guid>89e091b3489a50957b400a868f101d59</guid>
        </parent>
      </restore>
    </account>
    <account>
      <restore>
        <name>Commissions</name>
        <guid>41b9e953585cd5662712d62848e71789</guid>
        <type>EXPENSE</type>
        <currency>
          <space>ISO4217</space>
          <id>FRF</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>Money95mfunds fr</name>
        <guid>c7a4746248d0677c6421077833477bba</guid>
        <type>EXPENSE</type>
        <currency>
          <space>ISO4217</space>
          <id>FRF</id>
        </currency>
        <parent>
          <guid>41b9e953585cd5662712d62848e71789</guid>
        </parent>
      </restore>
    </account>
    <transaction>
      <restore>
        <guid>88ed4f76d273256aaa05eab3010731bf</guid>
        <date-posted>
          <s>1995-07-24 00:00:00 -0400</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:30:31 -0500</s>
          <ns>435420000</ns>
        </date-entered>
        <split>
          <guid>df25dcb351c89a20062a5ea5d294e8e8</guid>
          <reconcile-state>n</reconcile-state>
          <value>1828080/100</value>
          <quantity>6000000/100000</quantity>
          <account>e0e954412d6b31dfa1fcbb9937a125b0</account>
        </split>
        <split>
          <guid>a44971f955fa03a9318fff369962820a</guid>
          <reconcile-state>n</reconcile-state>
          <value>-2461820/100</value>
          <quantity>-2461820/100</quantity>
          <account>3bcb236af583d5bd7b6e0153fe97e7e3</account>
        </split>
        <split>
          <guid>1ec08747892bf80d01ff1dab46ccb150</guid>
          <reconcile-state>n</reconcile-state>
          <value>633735/100</value>
          <quantity>633735/100</quantity>
          <account>c7a4746248d0677c6421077833477bba</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>21f58d6ddf8e529905db462291963e68</guid>
        <date-posted>
          <s>1995-10-08 00:00:00 -0400</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:30:31 -0500</s>
          <ns>436816000</ns>
        </date-entered>
        <split>
          <guid>30f46d6ea24efb8f57ea386be57370c2</guid>
          <reconcile-state>n</reconcile-state>
          <value>3051000/100</value>
          <quantity>9000000/100000</quantity>
          <account>e0e954412d6b31dfa1fcbb9937a125b0</account>
        </split>
        <split>
          <guid>15160cb2e5b32864c451842cde8ffcd9</guid>
          <reconcile-state>n</reconcile-state>
          <value>-3254400/100</value>
          <quantity>-3254400/100</quantity>
          <account>3bcb236af583d5bd7b6e0153fe97e7e3</account>
        </split>
        <split>
          <guid>66a01b46ebec63544ca133efa6706f6b</guid>
          <reconcile-state>n</reconcile-state>
          <value>203400/100</value>
          <quantity>203400/100</quantity>
          <account>c7a4746248d0677c6421077833477bba</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>4cb943c207e102dbc22e5ef829bbc29d</guid>
        <date-posted>
          <s>1995-12-10 00:00:00 -0500</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:30:31 -0500</s>
          <ns>437989000</ns>
        </date-entered>
        <split>
          <guid>27a56b13c4d24744f3cbb53329120abc</guid>
          <reconcile-state>n</reconcile-state>
          <value>2053200/100</value>
          <quantity>6000000/100000</quantity>
          <account>e0e954412d6b31dfa1fcbb9937a125b0</account>
        </split>
        <split>
          <guid>19355bfa2d1208dd144aa7c32de06aa5</guid>
          <reconcile-state>n</reconcile-state>
          <value>-2053200/100</value>
          <quantity>-2053200/100</quantity>
          <account>3bcb236af583d5bd7b6e0153fe97e7e3</account>
        </split>
      </restore>
    </transaction>
  </ledger-data>
</gnc>
<!-- Local variables: -->
<!-- mode: xml        -->
<!-- End:             -->
