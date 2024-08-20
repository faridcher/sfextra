num2words <- function(x, unit=c('m^2','dm^2'), dec_sep=ifelse(length(unit)==2, ' و ', ' ممیز ')) {
  yekan <- c("", "یک", "دو", "سه", "چهار", "پنج", "شش", "هفت", "هشت", "نه" )
  dahyek <- c( "ده", "یازده", "دوازده", "سیزده", "چهارده", "پانزده", "شانزده", "هفده", "هجده", "نوزده" )
  dahgan <- c("", "بیست", "سی", "چهل", "پنجاه", "شصت", "هفتاد", "هشتاد", "نود")
  # dahgan-yekan (dh)
  dh <- apply(expand.grid(yekan, dahgan), 1, 
              function(a) paste0(rev(a), 
                                 collapse=ifelse(any(a==""),'', ' و ')))
  dh <- c(dh[1:10], dahyek, dh[11:length(dh)])

  sadgan <- c("", "یکصد", "دویست", "سیصد", "چهارصد", "پانصد", "ششصد", "هفتصد", "هشتصد", "نهصد" )
  # base 10 with exponents of 3 multiples: 3, 6, 9, 12,  etc.
  base10 <- c("", " هزار", " میلیون", " میلیارد", " تریلیون " )
  # indices correspond to the len of decimal places
  dec_places <- c("دهم", "صدم", "هزارم", "ده هزارم","صد هزارم")

  # unit translations
  ut <- c('m^2'='متر مربع', 'dm^2'='دسیمتر مربع', 'm'='متر', 'cm'='سانتیمتر')

  sapply(x, function(x1) {
  if(is.na(x1)) return(NA_character_) else if (x1==0) return("صفر")

  x1 <- if(length(unit)==2){
    # zs is the number of zeroes of the units divisions e.g. 1m^2/1dm^2=100 has 2 zeroes. 
    # zs is the minimum number of decimal places and is different from printf precision.
    zs <- log(units::set_units(1, unit[1], mode='standard')/units::set_units(1, unit[2], mode='standard'), 10)
    format(x1, nsmall=zs)
  }
  else as.character(x1)
  # digits character (dc) vector e.g. c("0","8") for 0.8
  # If units is a vector of two, the second units implies a min number of digits. For m-cm and sq m-dm are 2 and for kg-g is 3. Without converting 0.8 to 0.80, .08 and .8 become 'eight decimeters' because as.numeric(char) make them so.
  dc <- strsplit(x1, '\\.')[[1]]
  # words (ws) vector
  ws <- sapply(dc, .int2words)
  # units and separators
  if(length(unit)==1){
    # 82.24 هشتاد و دو ممیز بیست و چهار صدم
    w <- paste0(ws[ws!=''], collapse=dec_sep)
    # when x1 is integer with no decimal places
    dp <- if(length(dc)==1) '' else paste0(' ', dec_places[nchar(dc[2])])
    wdp <- paste0(w, dp)
    if(unit=='') wdp else paste(wdp, ut[unit], sep=' ')
  } else {
    # if x1<1 (e.g. 0.8) keep the second unit; if x1 is an integer keep the first and otherwise both
    # browser()
    uk <- if(dc[1]=='0') 2 else if(dc[2]=='00') 1 else seq_along(dc)
    u <- ut[unit[uk]]
    wsu <- paste(ws[ws!=''], u, sep=' ')
    paste0(wsu, collapse=dec_sep)
  }
})
}

# called by int2words
.hundreds <- function(x){
    s <- sadgan[x[3] + 1]
    dhx <- dh[x[2] *  10 + x[1] + 1] 
    paste(s, dhx , sep=ifelse(any(c(s, dhx)==""), "", " و "))
}

# called from num2words
.int2words <- function(x){
    # max n(umber) of digits is 14 (تریلیون)
    n <- 1:15
    d <- (as.numeric(x) %% 10^n) %/% 10^(n-1)
    m <- matrix(d, ncol=3, byrow=T)
    h <- apply(m, 1, .hundreds)
    paste0(rev(paste0(h[h!=""], base10[h!=""])), collapse=" و ")
}
