<?xml version="1.0"?>
<gnc>
  <version>1</version>
  <ledger-data>
    <account>
      <restore>
        <name>ABC Bank</name>
        <guid>eda56e3c0c402d1816f845430caf362e</guid>
        <type>BANK</type>
        <description>Some Old Bank Acct</description>
        <currency>
          <space>ISO4217</space>
          <id>USD</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>Swipe Brokers</name>
        <guid>729ae65a6c202a83a5335bcd32709888</guid>
        <type>BANK</type>
        <description>My Investment Account</description>
        <currency>
          <space>ISO4217</space>
          <id>USD</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>pocket cash</name>
        <guid>5d0b89cb9bcc5074cda514d0d4c10af4</guid>
        <type>CASH</type>
        <currency>
          <space>ISO4217</space>
          <id>USD</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>SlaveCardt</name>
        <guid>2b804a30e16e54a97da593fc1f71c386</guid>
        <type>CREDIT</type>
        <description>my credit card</description>
        <currency>
          <space>ISO4217</space>
          <id>USD</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>Gift Received</name>
        <guid>04b5cbc4192f4790fac0586c5cc152f4</guid>
        <type>INCOME</type>
        <description>Gift Received</description>
        <currency>
          <space>ISO4217</space>
          <id>USD</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>Invest Inc</name>
        <guid>d4f69b07cf7bb1a77c134f1b2c128684</guid>
        <type>INCOME</type>
        <description>Investment Income</description>
        <currency>
          <space>ISO4217</space>
          <id>USD</id>
        </currency>
      </restore>
    </account>
    <account>
      <restore>
        <name>Other Inc</name>
        <guid>0a241ed16a10c3be1836ce37f8a069e9</guid>
        <type>INCOME</type>
        <description>Other Income</description>
        <currency>
          <space>ISO4217</space>
          <id>USD</id>
        </currency>
      </restore>
    </account>
    <transaction>
      <restore>
        <guid>2277a65f698a0902c751d8d44db12eeb</guid>
        <num>DEP</num>
        <date-posted>
          <s>1997-06-17 00:00:00 -0400</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:55 -0500</s>
          <ns>994727000</ns>
        </date-entered>
        <description>from my pillow case</description>
        <split>
          <guid>12afd90bcb29753d5572ebec9b3a6fe8</guid>
          <memo>my memo</memo>
          <reconcile-state>n</reconcile-state>
          <value>-234500/100</value>
          <quantity>-234500/100</quantity>
          <account>0a241ed16a10c3be1836ce37f8a069e9</account>
        </split>
        <split>
          <guid>be0a8539128014e1f0d247eb54e8b07a</guid>
          <memo>my memo</memo>
          <reconcile-state>n</reconcile-state>
          <value>234500/100</value>
          <quantity>234500/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>62bb507b39f9cb5ce13cdbfe85d5dde9</guid>
        <date-posted>
          <s>1997-07-16 00:00:00 -0400</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:55 -0500</s>
          <ns>996065000</ns>
        </date-entered>
        <description>more from my pillow</description>
        <split>
          <guid>066346c0a87434491957713da1f3a3be</guid>
          <reconcile-state>n</reconcile-state>
          <value>-233300/100</value>
          <quantity>-233300/100</quantity>
          <account>04b5cbc4192f4790fac0586c5cc152f4</account>
        </split>
        <split>
          <guid>6552ad49f7202988165f0bfb0cab9f1d</guid>
          <reconcile-state>n</reconcile-state>
          <value>233300/100</value>
          <quantity>233300/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>ac62a8dedb6fafc4864d980e167d93b0</guid>
        <num>101</num>
        <date-posted>
          <s>1997-08-01 00:00:00 -0400</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:55 -0500</s>
          <ns>998739000</ns>
        </date-entered>
        <description>paycheck</description>
        <split>
          <guid>2f0880d0d28f0a38de0784f40f1364a1</guid>
          <memo>the boss paid me today!</memo>
          <reconcile-state>n</reconcile-state>
          <value>-54300/100</value>
          <quantity>-54300/100</quantity>
          <account>04b5cbc4192f4790fac0586c5cc152f4</account>
        </split>
        <split>
          <guid>f01abb3378f6a2c7dde4f4894540507b</guid>
          <memo>the boss paid me today!</memo>
          <reconcile-state>n</reconcile-state>
          <value>54300/100</value>
          <quantity>54300/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>5d33e9906feeb57fc22b723447c81605</guid>
        <num>DEP</num>
        <date-posted>
          <s>1997-08-01 00:00:00 -0400</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:55 -0500</s>
          <ns>997654000</ns>
        </date-entered>
        <description>put in more money</description>
        <split>
          <guid>cf74014d73e0ef079c0f10797b19ed55</guid>
          <memo>some other inc</memo>
          <reconcile-state>n</reconcile-state>
          <value>-30000/100</value>
          <quantity>-30000/100</quantity>
          <account>0a241ed16a10c3be1836ce37f8a069e9</account>
        </split>
        <split>
          <guid>686e0a76ab0ae02b10526729d4309509</guid>
          <memo>soem as invst</memo>
          <reconcile-state>n</reconcile-state>
          <value>-190000/100</value>
          <quantity>-190000/100</quantity>
          <account>d4f69b07cf7bb1a77c134f1b2c128684</account>
        </split>
        <split>
          <guid>8f08ab3bcae4fda2996a4d4ba58b2a65</guid>
          <memo>some as gift</memo>
          <reconcile-state>n</reconcile-state>
          <value>-110000/100</value>
          <quantity>-110000/100</quantity>
          <account>04b5cbc4192f4790fac0586c5cc152f4</account>
        </split>
        <split>
          <guid>008fcb736cbc9dda100dba888769caec</guid>
          <memo>some other inc</memo>
          <reconcile-state>n</reconcile-state>
          <value>330000/100</value>
          <quantity>330000/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>5e8d9293d5903ea18dc79e139b26dbc2</guid>
        <num>TXFR</num>
        <date-posted>
          <s>1997-09-12 00:00:00 -0400</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:56 -0500</s>
          <ns>9661000</ns>
        </date-entered>
        <description>move a pile of money to trading acct</description>
        <split>
          <guid>1630fe79d89d59a1c1b879dd705aaa41</guid>
          <memo>another memo</memo>
          <reconcile-state>n</reconcile-state>
          <value>500000/100</value>
          <quantity>500000/100</quantity>
          <account>729ae65a6c202a83a5335bcd32709888</account>
        </split>
        <split>
          <guid>6113d75cfcdea42bf1bf001df8e8ef8c</guid>
          <memo>another memo</memo>
          <reconcile-state>n</reconcile-state>
          <value>-500000/100</value>
          <quantity>-500000/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>5be926417dfce34c825504c32470f78f</guid>
        <num>TXFR</num>
        <date-posted>
          <s>1997-11-11 00:00:00 -0500</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:56 -0500</s>
          <ns>10837000</ns>
        </date-entered>
        <description>hal stock</description>
        <split>
          <guid>cd7d47aaee2a435c3901b3efd8e319fe</guid>
          <memo>income</memo>
          <reconcile-state>n</reconcile-state>
          <value>-1200/100</value>
          <quantity>-1200/100</quantity>
          <account>729ae65a6c202a83a5335bcd32709888</account>
        </split>
        <split>
          <guid>ceab9089fbca47c83a487f8ea5f06c50</guid>
          <memo>income</memo>
          <reconcile-state>n</reconcile-state>
          <value>1200/100</value>
          <quantity>1200/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>99fd0615f2db665f4bf90f438b2b5733</guid>
        <num>TXFR</num>
        <date-posted>
          <s>1997-11-11 00:00:00 -0500</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:56 -0500</s>
          <ns>11847000</ns>
        </date-entered>
        <description>hal stock</description>
        <split>
          <guid>bfa9de544784620d566abc08c6e91b3a</guid>
          <memo>income</memo>
          <reconcile-state>n</reconcile-state>
          <value>-1100/100</value>
          <quantity>-1100/100</quantity>
          <account>729ae65a6c202a83a5335bcd32709888</account>
        </split>
        <split>
          <guid>73a9e08aa5a660ca2fa2ab85acdff84f</guid>
          <memo>income</memo>
          <reconcile-state>n</reconcile-state>
          <value>1100/100</value>
          <quantity>1100/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>698e47a93b21837fd83babbfc5f6320d</guid>
        <num>TXFR</num>
        <date-posted>
          <s>1997-11-12 00:00:00 -0500</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:56 -0500</s>
          <ns>12862000</ns>
        </date-entered>
        <description>hal stock</description>
        <split>
          <guid>90e62018f0f3add84eb0ed68702ae942</guid>
          <memo>sold some stock!</memo>
          <reconcile-state>n</reconcile-state>
          <value>-50000/100</value>
          <quantity>-50000/100</quantity>
          <account>729ae65a6c202a83a5335bcd32709888</account>
        </split>
        <split>
          <guid>73fc5c8ae0c0468304bc9650d34f4194</guid>
          <memo>sold some stock!</memo>
          <reconcile-state>n</reconcile-state>
          <value>50000/100</value>
          <quantity>50000/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>249c2ccc0ada34b5027a64e0c18774c3</guid>
        <date-posted>
          <s>1997-11-28 00:00:00 -0500</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:56 -0500</s>
          <ns>13872000</ns>
        </date-entered>
        <description>pay the balance on the credit c</description>
        <split>
          <guid>d4b7da3a080da8fc44e75c4389790ada</guid>
          <memo>another memo</memo>
          <reconcile-state>n</reconcile-state>
          <value>44500/100</value>
          <quantity>44500/100</quantity>
          <account>2b804a30e16e54a97da593fc1f71c386</account>
        </split>
        <split>
          <guid>51fc2af007ad07027dfbe3ba4d4b6304</guid>
          <memo>another memo</memo>
          <reconcile-state>n</reconcile-state>
          <value>-44500/100</value>
          <quantity>-44500/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
    <transaction>
      <restore>
        <guid>b1a6b7fad984e769bd6c3dd56abcfbe8</guid>
        <date-posted>
          <s>1997-11-28 00:00:00 -0500</s>
        </date-posted>
        <date-entered>
          <s>2001-03-26 15:32:56 -0500</s>
          <ns>14844000</ns>
        </date-entered>
        <description>cash in my pocket</description>
        <split>
          <guid>c862917ad2379a5710b1431c395e658b</guid>
          <reconcile-state>n</reconcile-state>
          <value>4500/100</value>
          <quantity>4500/100</quantity>
          <account>5d0b89cb9bcc5074cda514d0d4c10af4</account>
        </split>
        <split>
          <guid>b4ceb078101fd7b78f4a0a2c9d3e2768</guid>
          <reconcile-state>n</reconcile-state>
          <value>-4500/100</value>
          <quantity>-4500/100</quantity>
          <account>eda56e3c0c402d1816f845430caf362e</account>
        </split>
      </restore>
    </transaction>
  </ledger-data>
</gnc>
<!-- Local variables: -->
<!-- mode: xml        -->
<!-- End:             -->
