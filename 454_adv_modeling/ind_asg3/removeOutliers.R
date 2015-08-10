removeOutliers = function(spambase) {
  
  x = spambase %>% select(word_freq_make) %>% filter(word_freq_make > 0)
  q1 = quantile(x$word_freq_make, c(0.25))
  q3 = quantile(x$word_freq_make, c(0.75))
  iqr = IQR(x$word_freq_make)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_make"] > 0) {
      if (spambase[i, "word_freq_make"] < lb) {
        spambase[i, "word_freq_make"] = lb
      }
      else if(spambase[i, "word_freq_make"] > ub) {
        spambase[i, "word_freq_make"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_address) %>% filter(word_freq_address > 0)
  q1 = quantile(x$word_freq_address, c(0.25))
  q3 = quantile(x$word_freq_address, c(0.75))
  iqr = IQR(x$word_freq_address)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_address"] > 0) {
      if (spambase[i, "word_freq_address"] < lb) {
        spambase[i, "word_freq_address"] = lb
      }
      else if(spambase[i, "word_freq_address"] > ub) {
        spambase[i, "word_freq_address"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_all) %>% filter(word_freq_all > 0)
  q1 = quantile(x$word_freq_all, c(0.25))
  q3 = quantile(x$word_freq_all, c(0.75))
  iqr = IQR(x$word_freq_all)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_all"] > 0) {
      if (spambase[i, "word_freq_all"] < lb) {
        spambase[i, "word_freq_all"] = lb
      }
      else if(spambase[i, "word_freq_all"] > ub) {
        spambase[i, "word_freq_all"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_3d) %>% filter(word_freq_3d > 0)
  q1 = quantile(x$word_freq_3d, c(0.25))
  q3 = quantile(x$word_freq_3d, c(0.75))
  iqr = IQR(x$word_freq_3d)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_3d"] > 0) {
      if (spambase[i, "word_freq_3d"] < lb) {
        spambase[i, "word_freq_3d"] = lb
      }
      else if(spambase[i, "word_freq_3d"] > ub) {
        spambase[i, "word_freq_3d"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_our) %>% filter(word_freq_our > 0)
  q1 = quantile(x$word_freq_our, c(0.25))
  q3 = quantile(x$word_freq_our, c(0.75))
  iqr = IQR(x$word_freq_our)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_our"] > 0) {
      if (spambase[i, "word_freq_our"] < lb) {
        spambase[i, "word_freq_our"] = lb
      }
      else if(spambase[i, "word_freq_our"] > ub) {
        spambase[i, "word_freq_our"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_over) %>% filter(word_freq_over > 0)
  q1 = quantile(x$word_freq_over, c(0.25))
  q3 = quantile(x$word_freq_over, c(0.75))
  iqr = IQR(x$word_freq_over)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_over"] > 0) {
      if (spambase[i, "word_freq_over"] < lb) {
        spambase[i, "word_freq_over"] = lb
      }
      else if(spambase[i, "word_freq_over"] > ub) {
        spambase[i, "word_freq_over"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_remove) %>% filter(word_freq_remove > 0)
  q1 = quantile(x$word_freq_remove, c(0.25))
  q3 = quantile(x$word_freq_remove, c(0.75))
  iqr = IQR(x$word_freq_remove)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_remove"] > 0) {
      if (spambase[i, "word_freq_remove"] < lb) {
        spambase[i, "word_freq_remove"] = lb
      }
      else if(spambase[i, "word_freq_remove"] > ub) {
        spambase[i, "word_freq_remove"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_internet) %>% filter(word_freq_internet > 0)
  q1 = quantile(x$word_freq_internet, c(0.25))
  q3 = quantile(x$word_freq_internet, c(0.75))
  iqr = IQR(x$word_freq_internet)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_internet"] > 0) {
      if (spambase[i, "word_freq_internet"] < lb) {
        spambase[i, "word_freq_internet"] = lb
      }
      else if(spambase[i, "word_freq_internet"] > ub) {
        spambase[i, "word_freq_internet"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_order) %>% filter(word_freq_order > 0)
  q1 = quantile(x$word_freq_order, c(0.25))
  q3 = quantile(x$word_freq_order, c(0.75))
  iqr = IQR(x$word_freq_order)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_order"] > 0) {
      if (spambase[i, "word_freq_order"] < lb) {
        spambase[i, "word_freq_order"] = lb
      }
      else if(spambase[i, "word_freq_order"] > ub) {
        spambase[i, "word_freq_order"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_mail) %>% filter(word_freq_mail > 0)
  q1 = quantile(x$word_freq_mail, c(0.25))
  q3 = quantile(x$word_freq_mail, c(0.75))
  iqr = IQR(x$word_freq_mail)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_mail"] > 0) {
      if (spambase[i, "word_freq_mail"] < lb) {
        spambase[i, "word_freq_mail"] = lb
      }
      else if(spambase[i, "word_freq_mail"] > ub) {
        spambase[i, "word_freq_mail"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_receive) %>% filter(word_freq_receive > 0)
  q1 = quantile(x$word_freq_receive, c(0.25))
  q3 = quantile(x$word_freq_receive, c(0.75))
  iqr = IQR(x$word_freq_receive)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_receive"] > 0) {
      if (spambase[i, "word_freq_receive"] < lb) {
        spambase[i, "word_freq_receive"] = lb
      }
      else if(spambase[i, "word_freq_receive"] > ub) {
        spambase[i, "word_freq_receive"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_will) %>% filter(word_freq_will > 0)
  q1 = quantile(x$word_freq_will, c(0.25))
  q3 = quantile(x$word_freq_will, c(0.75))
  iqr = IQR(x$word_freq_will)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_will"] > 0) {
      if (spambase[i, "word_freq_will"] < lb) {
        spambase[i, "word_freq_will"] = lb
      }
      else if(spambase[i, "word_freq_will"] > ub) {
        spambase[i, "word_freq_will"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_people) %>% filter(word_freq_people > 0)
  q1 = quantile(x$word_freq_people, c(0.25))
  q3 = quantile(x$word_freq_people, c(0.75))
  iqr = IQR(x$word_freq_people)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_people"] > 0) {
      if (spambase[i, "word_freq_people"] < lb) {
        spambase[i, "word_freq_people"] = lb
      }
      else if(spambase[i, "word_freq_people"] > ub) {
        spambase[i, "word_freq_people"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_report) %>% filter(word_freq_report > 0)
  q1 = quantile(x$word_freq_report, c(0.25))
  q3 = quantile(x$word_freq_report, c(0.75))
  iqr = IQR(x$word_freq_report)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_report"] > 0) {
      if (spambase[i, "word_freq_report"] < lb) {
        spambase[i, "word_freq_report"] = lb
      }
      else if(spambase[i, "word_freq_report"] > ub) {
        spambase[i, "word_freq_report"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_addresses) %>% filter(word_freq_addresses > 0)
  q1 = quantile(x$word_freq_addresses, c(0.25))
  q3 = quantile(x$word_freq_addresses, c(0.75))
  iqr = IQR(x$word_freq_addresses)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_addresses"] > 0) {
      if (spambase[i, "word_freq_addresses"] < lb) {
        spambase[i, "word_freq_addresses"] = lb
      }
      else if(spambase[i, "word_freq_addresses"] > ub) {
        spambase[i, "word_freq_addresses"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_free) %>% filter(word_freq_free > 0)
  q1 = quantile(x$word_freq_free, c(0.25))
  q3 = quantile(x$word_freq_free, c(0.75))
  iqr = IQR(x$word_freq_free)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_free"] > 0) {
      if (spambase[i, "word_freq_free"] < lb) {
        spambase[i, "word_freq_free"] = lb
      }
      else if(spambase[i, "word_freq_free"] > ub) {
        spambase[i, "word_freq_free"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_business) %>% filter(word_freq_business > 0)
  q1 = quantile(x$word_freq_business, c(0.25))
  q3 = quantile(x$word_freq_business, c(0.75))
  iqr = IQR(x$word_freq_business)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_business"] > 0) {
      if (spambase[i, "word_freq_business"] < lb) {
        spambase[i, "word_freq_business"] = lb
      }
      else if(spambase[i, "word_freq_business"] > ub) {
        spambase[i, "word_freq_business"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_email) %>% filter(word_freq_email > 0)
  q1 = quantile(x$word_freq_email, c(0.25))
  q3 = quantile(x$word_freq_email, c(0.75))
  iqr = IQR(x$word_freq_email)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_email"] > 0) {
      if (spambase[i, "word_freq_email"] < lb) {
        spambase[i, "word_freq_email"] = lb
      }
      else if(spambase[i, "word_freq_email"] > ub) {
        spambase[i, "word_freq_email"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_you) %>% filter(word_freq_you > 0)
  q1 = quantile(x$word_freq_you, c(0.25))
  q3 = quantile(x$word_freq_you, c(0.75))
  iqr = IQR(x$word_freq_you)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_you"] > 0) {
      if (spambase[i, "word_freq_you"] < lb) {
        spambase[i, "word_freq_you"] = lb
      }
      else if(spambase[i, "word_freq_you"] > ub) {
        spambase[i, "word_freq_you"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_credit) %>% filter(word_freq_credit > 0)
  q1 = quantile(x$word_freq_credit, c(0.25))
  q3 = quantile(x$word_freq_credit, c(0.75))
  iqr = IQR(x$word_freq_credit)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_credit"] > 0) {
      if (spambase[i, "word_freq_credit"] < lb) {
        spambase[i, "word_freq_credit"] = lb
      }
      else if(spambase[i, "word_freq_credit"] > ub) {
        spambase[i, "word_freq_credit"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_your) %>% filter(word_freq_your > 0)
  q1 = quantile(x$word_freq_your, c(0.25))
  q3 = quantile(x$word_freq_your, c(0.75))
  iqr = IQR(x$word_freq_your)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_your"] > 0) {
      if (spambase[i, "word_freq_your"] < lb) {
        spambase[i, "word_freq_your"] = lb
      }
      else if(spambase[i, "word_freq_your"] > ub) {
        spambase[i, "word_freq_your"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_font) %>% filter(word_freq_font > 0)
  q1 = quantile(x$word_freq_font, c(0.25))
  q3 = quantile(x$word_freq_font, c(0.75))
  iqr = IQR(x$word_freq_font)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_font"] > 0) {
      if (spambase[i, "word_freq_font"] < lb) {
        spambase[i, "word_freq_font"] = lb
      }
      else if(spambase[i, "word_freq_font"] > ub) {
        spambase[i, "word_freq_font"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_000) %>% filter(word_freq_000 > 0)
  q1 = quantile(x$word_freq_000, c(0.25))
  q3 = quantile(x$word_freq_000, c(0.75))
  iqr = IQR(x$word_freq_000)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_000"] > 0) {
      if (spambase[i, "word_freq_000"] < lb) {
        spambase[i, "word_freq_000"] = lb
      }
      else if(spambase[i, "word_freq_000"] > ub) {
        spambase[i, "word_freq_000"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_money) %>% filter(word_freq_money > 0)
  q1 = quantile(x$word_freq_money, c(0.25))
  q3 = quantile(x$word_freq_money, c(0.75))
  iqr = IQR(x$word_freq_money)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_money"] > 0) {
      if (spambase[i, "word_freq_money"] < lb) {
        spambase[i, "word_freq_money"] = lb
      }
      else if(spambase[i, "word_freq_money"] > ub) {
        spambase[i, "word_freq_money"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_hp) %>% filter(word_freq_hp > 0)
  q1 = quantile(x$word_freq_hp, c(0.25))
  q3 = quantile(x$word_freq_hp, c(0.75))
  iqr = IQR(x$word_freq_hp)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_hp"] > 0) {
      if (spambase[i, "word_freq_hp"] < lb) {
        spambase[i, "word_freq_hp"] = lb
      }
      else if(spambase[i, "word_freq_hp"] > ub) {
        spambase[i, "word_freq_hp"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_hpl) %>% filter(word_freq_hpl > 0)
  q1 = quantile(x$word_freq_hpl, c(0.25))
  q3 = quantile(x$word_freq_hpl, c(0.75))
  iqr = IQR(x$word_freq_hpl)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_hpl"] > 0) {
      if (spambase[i, "word_freq_hpl"] < lb) {
        spambase[i, "word_freq_hpl"] = lb
      }
      else if(spambase[i, "word_freq_hpl"] > ub) {
        spambase[i, "word_freq_hpl"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_george) %>% filter(word_freq_george > 0)
  q1 = quantile(x$word_freq_george, c(0.25))
  q3 = quantile(x$word_freq_george, c(0.75))
  iqr = IQR(x$word_freq_george)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_george"] > 0) {
      if (spambase[i, "word_freq_george"] < lb) {
        spambase[i, "word_freq_george"] = lb
      }
      else if(spambase[i, "word_freq_george"] > ub) {
        spambase[i, "word_freq_george"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_650) %>% filter(word_freq_650 > 0)
  q1 = quantile(x$word_freq_650, c(0.25))
  q3 = quantile(x$word_freq_650, c(0.75))
  iqr = IQR(x$word_freq_650)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_650"] > 0) {
      if (spambase[i, "word_freq_650"] < lb) {
        spambase[i, "word_freq_650"] = lb
      }
      else if(spambase[i, "word_freq_650"] > ub) {
        spambase[i, "word_freq_650"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_lab) %>% filter(word_freq_lab > 0)
  q1 = quantile(x$word_freq_lab, c(0.25))
  q3 = quantile(x$word_freq_lab, c(0.75))
  iqr = IQR(x$word_freq_lab)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_lab"] > 0) {
      if (spambase[i, "word_freq_lab"] < lb) {
        spambase[i, "word_freq_lab"] = lb
      }
      else if(spambase[i, "word_freq_lab"] > ub) {
        spambase[i, "word_freq_lab"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_labs) %>% filter(word_freq_labs > 0)
  q1 = quantile(x$word_freq_labs, c(0.25))
  q3 = quantile(x$word_freq_labs, c(0.75))
  iqr = IQR(x$word_freq_labs)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_labs"] > 0) {
      if (spambase[i, "word_freq_labs"] < lb) {
        spambase[i, "word_freq_labs"] = lb
      }
      else if(spambase[i, "word_freq_labs"] > ub) {
        spambase[i, "word_freq_labs"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_telnet) %>% filter(word_freq_telnet > 0)
  q1 = quantile(x$word_freq_telnet, c(0.25))
  q3 = quantile(x$word_freq_telnet, c(0.75))
  iqr = IQR(x$word_freq_telnet)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_telnet"] > 0) {
      if (spambase[i, "word_freq_telnet"] < lb) {
        spambase[i, "word_freq_telnet"] = lb
      }
      else if(spambase[i, "word_freq_telnet"] > ub) {
        spambase[i, "word_freq_telnet"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_857) %>% filter(word_freq_857 > 0)
  q1 = quantile(x$word_freq_857, c(0.25))
  q3 = quantile(x$word_freq_857, c(0.75))
  iqr = IQR(x$word_freq_857)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_857"] > 0) {
      if (spambase[i, "word_freq_857"] < lb) {
        spambase[i, "word_freq_857"] = lb
      }
      else if(spambase[i, "word_freq_857"] > ub) {
        spambase[i, "word_freq_857"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_data) %>% filter(word_freq_data > 0)
  q1 = quantile(x$word_freq_data, c(0.25))
  q3 = quantile(x$word_freq_data, c(0.75))
  iqr = IQR(x$word_freq_data)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_data"] > 0) {
      if (spambase[i, "word_freq_data"] < lb) {
        spambase[i, "word_freq_data"] = lb
      }
      else if(spambase[i, "word_freq_data"] > ub) {
        spambase[i, "word_freq_data"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_415) %>% filter(word_freq_415 > 0)
  q1 = quantile(x$word_freq_415, c(0.25))
  q3 = quantile(x$word_freq_415, c(0.75))
  iqr = IQR(x$word_freq_415)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_415"] > 0) {
      if (spambase[i, "word_freq_415"] < lb) {
        spambase[i, "word_freq_415"] = lb
      }
      else if(spambase[i, "word_freq_415"] > ub) {
        spambase[i, "word_freq_415"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_85) %>% filter(word_freq_85 > 0)
  q1 = quantile(x$word_freq_85, c(0.25))
  q3 = quantile(x$word_freq_85, c(0.75))
  iqr = IQR(x$word_freq_85)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_85"] > 0) {
      if (spambase[i, "word_freq_85"] < lb) {
        spambase[i, "word_freq_85"] = lb
      }
      else if(spambase[i, "word_freq_85"] > ub) {
        spambase[i, "word_freq_85"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_technology) %>% filter(word_freq_technology > 0)
  q1 = quantile(x$word_freq_technology, c(0.25))
  q3 = quantile(x$word_freq_technology, c(0.75))
  iqr = IQR(x$word_freq_technology)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_technology"] > 0) {
      if (spambase[i, "word_freq_technology"] < lb) {
        spambase[i, "word_freq_technology"] = lb
      }
      else if(spambase[i, "word_freq_technology"] > ub) {
        spambase[i, "word_freq_technology"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_1999) %>% filter(word_freq_1999 > 0)
  q1 = quantile(x$word_freq_1999, c(0.25))
  q3 = quantile(x$word_freq_1999, c(0.75))
  iqr = IQR(x$word_freq_1999)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_1999"] > 0) {
      if (spambase[i, "word_freq_1999"] < lb) {
        spambase[i, "word_freq_1999"] = lb
      }
      else if(spambase[i, "word_freq_1999"] > ub) {
        spambase[i, "word_freq_1999"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_parts) %>% filter(word_freq_parts > 0)
  q1 = quantile(x$word_freq_parts, c(0.25))
  q3 = quantile(x$word_freq_parts, c(0.75))
  iqr = IQR(x$word_freq_parts)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_parts"] > 0) {
      if (spambase[i, "word_freq_parts"] < lb) {
        spambase[i, "word_freq_parts"] = lb
      }
      else if(spambase[i, "word_freq_parts"] > ub) {
        spambase[i, "word_freq_parts"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_pm) %>% filter(word_freq_pm > 0)
  q1 = quantile(x$word_freq_pm, c(0.25))
  q3 = quantile(x$word_freq_pm, c(0.75))
  iqr = IQR(x$word_freq_pm)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_pm"] > 0) {
      if (spambase[i, "word_freq_pm"] < lb) {
        spambase[i, "word_freq_pm"] = lb
      }
      else if(spambase[i, "word_freq_pm"] > ub) {
        spambase[i, "word_freq_pm"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_direct) %>% filter(word_freq_direct > 0)
  q1 = quantile(x$word_freq_direct, c(0.25))
  q3 = quantile(x$word_freq_direct, c(0.75))
  iqr = IQR(x$word_freq_direct)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_direct"] > 0) {
      if (spambase[i, "word_freq_direct"] < lb) {
        spambase[i, "word_freq_direct"] = lb
      }
      else if(spambase[i, "word_freq_direct"] > ub) {
        spambase[i, "word_freq_direct"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_cs) %>% filter(word_freq_cs > 0)
  q1 = quantile(x$word_freq_cs, c(0.25))
  q3 = quantile(x$word_freq_cs, c(0.75))
  iqr = IQR(x$word_freq_cs)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_cs"] > 0) {
      if (spambase[i, "word_freq_cs"] < lb) {
        spambase[i, "word_freq_cs"] = lb
      }
      else if(spambase[i, "word_freq_cs"] > ub) {
        spambase[i, "word_freq_cs"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_meeting) %>% filter(word_freq_meeting > 0)
  q1 = quantile(x$word_freq_meeting, c(0.25))
  q3 = quantile(x$word_freq_meeting, c(0.75))
  iqr = IQR(x$word_freq_meeting)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_meeting"] > 0) {
      if (spambase[i, "word_freq_meeting"] < lb) {
        spambase[i, "word_freq_meeting"] = lb
      }
      else if(spambase[i, "word_freq_meeting"] > ub) {
        spambase[i, "word_freq_meeting"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_original) %>% filter(word_freq_original > 0)
  q1 = quantile(x$word_freq_original, c(0.25))
  q3 = quantile(x$word_freq_original, c(0.75))
  iqr = IQR(x$word_freq_original)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_original"] > 0) {
      if (spambase[i, "word_freq_original"] < lb) {
        spambase[i, "word_freq_original"] = lb
      }
      else if(spambase[i, "word_freq_original"] > ub) {
        spambase[i, "word_freq_original"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_project) %>% filter(word_freq_project > 0)
  q1 = quantile(x$word_freq_project, c(0.25))
  q3 = quantile(x$word_freq_project, c(0.75))
  iqr = IQR(x$word_freq_project)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_project"] > 0) {
      if (spambase[i, "word_freq_project"] < lb) {
        spambase[i, "word_freq_project"] = lb
      }
      else if(spambase[i, "word_freq_project"] > ub) {
        spambase[i, "word_freq_project"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_re) %>% filter(word_freq_re > 0)
  q1 = quantile(x$word_freq_re, c(0.25))
  q3 = quantile(x$word_freq_re, c(0.75))
  iqr = IQR(x$word_freq_re)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_re"] > 0) {
      if (spambase[i, "word_freq_re"] < lb) {
        spambase[i, "word_freq_re"] = lb
      }
      else if(spambase[i, "word_freq_re"] > ub) {
        spambase[i, "word_freq_re"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_edu) %>% filter(word_freq_edu > 0)
  q1 = quantile(x$word_freq_edu, c(0.25))
  q3 = quantile(x$word_freq_edu, c(0.75))
  iqr = IQR(x$word_freq_edu)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_edu"] > 0) {
      if (spambase[i, "word_freq_edu"] < lb) {
        spambase[i, "word_freq_edu"] = lb
      }
      else if(spambase[i, "word_freq_edu"] > ub) {
        spambase[i, "word_freq_edu"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_table) %>% filter(word_freq_table > 0)
  q1 = quantile(x$word_freq_table, c(0.25))
  q3 = quantile(x$word_freq_table, c(0.75))
  iqr = IQR(x$word_freq_table)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_table"] > 0) {
      if (spambase[i, "word_freq_table"] < lb) {
        spambase[i, "word_freq_table"] = lb
      }
      else if(spambase[i, "word_freq_table"] > ub) {
        spambase[i, "word_freq_table"] = ub
      }
    }
  }
  
  x = spambase %>% select(word_freq_conference) %>% filter(word_freq_conference > 0)
  q1 = quantile(x$word_freq_conference, c(0.25))
  q3 = quantile(x$word_freq_conference, c(0.75))
  iqr = IQR(x$word_freq_conference)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "word_freq_conference"] > 0) {
      if (spambase[i, "word_freq_conference"] < lb) {
        spambase[i, "word_freq_conference"] = lb
      }
      else if(spambase[i, "word_freq_conference"] > ub) {
        spambase[i, "word_freq_conference"] = ub
      }
    }
  }
  
  x = spambase %>% select(char_freq_semicolon) %>% filter(char_freq_semicolon > 0)
  q1 = quantile(x$char_freq_semicolon, c(0.25))
  q3 = quantile(x$char_freq_semicolon, c(0.75))
  iqr = IQR(x$char_freq_semicolon)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "char_freq_semicolon"] > 0) {
      if (spambase[i, "char_freq_semicolon"] < lb) {
        spambase[i, "char_freq_semicolon"] = lb
      }
      else if(spambase[i, "char_freq_semicolon"] > ub) {
        spambase[i, "char_freq_semicolon"] = ub
      }
    }
  }
  
  x = spambase %>% select(char_freq_leftparen) %>% filter(char_freq_leftparen > 0)
  q1 = quantile(x$char_freq_leftparen, c(0.25))
  q3 = quantile(x$char_freq_leftparen, c(0.75))
  iqr = IQR(x$char_freq_leftparen)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "char_freq_leftparen"] > 0) {
      if (spambase[i, "char_freq_leftparen"] < lb) {
        spambase[i, "char_freq_leftparen"] = lb
      }
      else if(spambase[i, "char_freq_leftparen"] > ub) {
        spambase[i, "char_freq_leftparen"] = ub
      }
    }
  }
  
  x = spambase %>% select(char_freq_leftsqbrack) %>% filter(char_freq_leftsqbrack > 0)
  q1 = quantile(x$char_freq_leftsqbrack, c(0.25))
  q3 = quantile(x$char_freq_leftsqbrack, c(0.75))
  iqr = IQR(x$char_freq_leftsqbrack)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "char_freq_leftsqbrack"] > 0) {
      if (spambase[i, "char_freq_leftsqbrack"] < lb) {
        spambase[i, "char_freq_leftsqbrack"] = lb
      }
      else if(spambase[i, "char_freq_leftsqbrack"] > ub) {
        spambase[i, "char_freq_leftsqbrack"] = ub
      }
    }
  }
  
  x = spambase %>% select(char_freq_excl) %>% filter(char_freq_excl > 0)
  q1 = quantile(x$char_freq_excl, c(0.25))
  q3 = quantile(x$char_freq_excl, c(0.75))
  iqr = IQR(x$char_freq_excl)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "char_freq_excl"] > 0) {
      if (spambase[i, "char_freq_excl"] < lb) {
        spambase[i, "char_freq_excl"] = lb
      }
      else if(spambase[i, "char_freq_excl"] > ub) {
        spambase[i, "char_freq_excl"] = ub
      }
    }
  }
  
  x = spambase %>% select(char_freq_dollar) %>% filter(char_freq_dollar > 0)
  q1 = quantile(x$char_freq_dollar, c(0.25))
  q3 = quantile(x$char_freq_dollar, c(0.75))
  iqr = IQR(x$char_freq_dollar)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "char_freq_dollar"] > 0) {
      if (spambase[i, "char_freq_dollar"] < lb) {
        spambase[i, "char_freq_dollar"] = lb
      }
      else if(spambase[i, "char_freq_dollar"] > ub) {
        spambase[i, "char_freq_dollar"] = ub
      }
    }
  }
  
  x = spambase %>% select(char_freq_hash) %>% filter(char_freq_hash > 0)
  q1 = quantile(x$char_freq_hash, c(0.25))
  q3 = quantile(x$char_freq_hash, c(0.75))
  iqr = IQR(x$char_freq_hash)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "char_freq_hash"] > 0) {
      if (spambase[i, "char_freq_hash"] < lb) {
        spambase[i, "char_freq_hash"] = lb
      }
      else if(spambase[i, "char_freq_hash"] > ub) {
        spambase[i, "char_freq_hash"] = ub
      }
    }
  }
  
  x = spambase %>% select(capital_run_length_average) %>% filter(capital_run_length_average > 0)
  q1 = quantile(x$capital_run_length_average, c(0.25))
  q3 = quantile(x$capital_run_length_average, c(0.75))
  iqr = IQR(x$capital_run_length_average)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "capital_run_length_average"] > 0) {
      if (spambase[i, "capital_run_length_average"] < lb) {
        spambase[i, "capital_run_length_average"] = lb
      }
      else if(spambase[i, "capital_run_length_average"] > ub) {
        spambase[i, "capital_run_length_average"] = ub
      }
    }
  }
  
  x = spambase %>% select(capital_run_length_longest) %>% filter(capital_run_length_longest > 0)
  q1 = quantile(x$capital_run_length_longest, c(0.25))
  q3 = quantile(x$capital_run_length_longest, c(0.75))
  iqr = IQR(x$capital_run_length_longest)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "capital_run_length_longest"] > 0) {
      if (spambase[i, "capital_run_length_longest"] < lb) {
        spambase[i, "capital_run_length_longest"] = lb
      }
      else if(spambase[i, "capital_run_length_longest"] > ub) {
        spambase[i, "capital_run_length_longest"] = ub
      }
    }
  }
  
  x = spambase %>% select(capital_run_length_total) %>% filter(capital_run_length_total > 0)
  q1 = quantile(x$capital_run_length_total, c(0.25))
  q3 = quantile(x$capital_run_length_total, c(0.75))
  iqr = IQR(x$capital_run_length_total)
  lb = q1 - 1.5*iqr
  ub = q3 + 1.5*iqr
  for (i in 1:nrow(spambase)) {
    if (spambase[i, "capital_run_length_total"] > 0) {
      if (spambase[i, "capital_run_length_total"] < lb) {
        spambase[i, "capital_run_length_total"] = lb
      }
      else if(spambase[i, "capital_run_length_total"] > ub) {
        spambase[i, "capital_run_length_total"] = ub
      }
    }
  }
  
  return (spambase)
}