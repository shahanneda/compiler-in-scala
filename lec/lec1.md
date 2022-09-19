
31 regular register (labeled from 1 to 31)

32 = LO register
33 = HI register
34 = PC register


Scala tip: match is like switch

Reason why we increment before execute:
- Some insturtions may set to PC to something, by first incrementing, we won't change the value that is set. (For example, if we incrmented last, the instrution would set the PC to a certain value, but then it would be incremented right after)