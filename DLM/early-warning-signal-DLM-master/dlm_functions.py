import numpy as np
import scipy.linalg
from scipy.stats import t as tdstr
from scipy.stats import norm
from scipy.interpolate import interp1d
from datetime import date, datetime, timedelta
from dateutil.relativedelta import relativedelta
import matplotlib
matplotlib.rc('font', size=14)
import matplotlib.pyplot as plt


class Prior:
    def __init__(self, m, C, S, nu): 
        self.m = m # mean of t-distribution 隐变量的均值
        self.C = C # scale matrix of t-distribution 隐变量的t分布方差
        self.S = S # precision ~ IG(nu/2,S*nu/2) 随机误差的分布方差 Assuming the variation of observational error follows an inverse-gamma (IG) distribution,
        self.nu = nu # degree of freedom 随机误差的自由度

class Model:
    def __init__(self,Y,X,rseas,delta):
        self.Y = Y  # NDVI 1:374
        self.X = X  # NDVI 0:373 + Precp 0:373
        self.rseas = rseas # [1, 2]
        dd = np.ones(4)*delta #[0.98,0.98,0.98,0.98]
        self.delta = dd  #[0.98,0.98,0.98,0.98]
        
        ntrend = 2 # local trend
        nregn = X.shape[1] # 2
        pseas = len(rseas) # 2
        nseas = pseas*2     # 4
        
        m = np.zeros([ntrend+nregn+nseas,1]) # mean of t-distribution [2 + 2 + 4,1] 全为0的列向量 均值
        # 自相关, 回归, 周期性
        # np.zeros([2+2+4,1])

        C = scipy.linalg.block_diag(1*np.eye(ntrend),1*np.eye(nregn),1*np.eye(nseas)) # scale matrix of t-distribution 方差
        # 8x8的全为1的对角矩阵
        # scipy.linalg.block_diag(1*np.eye(2),1*np.eye(2),1*np.eye(4)) 
        
        S = np.power(0.2,2) # precision ~ IG(nu/2,S*nu/2)  0.04
        
        nu = ntrend+nregn+pseas  # degree of freedom 2 + 2 + 4
        
        pr = Prior(m,C,S,nu)
        self.prior = pr
        

def forwardFilteringM(Model):
    # All the parameters estimated here correspond Eqs. 13-16 and the related ones in the Supplementary Information of Liu et al. (2019)
    # notation in the code -> notation in Liu et al., 2019: 
    # m -> m_t; C -> C_t^{**}; nu -> n_t; 
    # a -> a_t; R -> R_t^{**}; F -> F_t; e -> e_t; y -> y_t; Q -> q_t^{**}; f -> f_t; S -> s_t = d_t/n_t
    
    Y = Model.Y # NDVI 1:374
    X = Model.X # NDVI 0:373 + Precp 0:373
    rseas = Model.rseas # [1, 2]
    delta = Model.delta #[0.98,0.98,0.98,0.98]
    Prior = Model.prior # m 全为0的列向量 均值; C: 全为1的对角矩阵; precision: 0.04; # degree of freedom
    period = 1 # 365.25/16
    
    
    # 全为0.8 
    deltrend = delta[0];delregn = delta[1];delseas = delta[2];delvar = delta[3]
    
    # 关联隐变量的矩阵以及隐变量之间的自相关转换矩阵
    # local mean and trend
    Ftrend = np.array([[1],[0]])
    Gtrend = np.array([[1,1],[0,1]])
    
    # 自相关和环境影响,回归的影响 (Y, Precipitation)2个解释的因变量
    nregn = X.shape[1]; Fregn = np.zeros([nregn,1])
    Gregn=np.eye(nregn)
    
    # 季节周期动态
    pseas = len(rseas); Fseas = np.tile([[1],[0]],[pseas,1])
    
    # 对于每个季节分量 按周期分别进行傅立叶转化
    nseas = pseas*2
    Gseas = np.zeros([nseas,nseas]) # 4x4的0矩阵
    for j in range(pseas):
        c = np.cos(2*np.pi*rseas[j]/period);
        s = np.sin(2*np.pi*rseas[j]/period);
        i = np.arange(2*j,2*(j+1))
        Gseas[np.reshape(i,[2,1]),i] = [[c,s],[-s,c]]
    
    # 记录合并后矩阵的位置
    ntrend = len(Ftrend) # 2
    # local mean and trend
    itrend = np.arange(0,ntrend) # 
    # regression
    iregn = np.arange(ntrend,ntrend+nregn)
    # season pattern
    iseas = np.arange(ntrend+nregn,ntrend+nregn+nseas)
    
    
    # 合并所有转换矩阵, 设置起始状态以及数据存储空间
    F = np.concatenate((Ftrend,Fregn,Fseas),axis=0)
    G = scipy.linalg.block_diag(Gtrend,Gregn,Gseas) # 合并所有的对角矩阵
    m = Prior.m # m 全为0的列向量 均值; 
    C = Prior.C # C: 全为1的对角矩阵; 
    S = Prior.S # precision: 0.04;
    nu = Prior.nu # degree of freedom

    # 设置数据存储空间
    T = len(Y) # Y的时间序列
    sm = np.zeros(m.shape) # 隐变量均值 (自相关效应)
    sC = np.zeros([C.shape[0],C.shape[1],1]) # 隐变量方差 (自相关效应)
    sS = np.zeros(1) # precision
    snu = np.zeros(1) # freedom
    slik = np.zeros(1) # likelihood
    
    for t in range(T):
        
        a = np.dot(G,m) # at = G mt−1: t时刻隐变量的均值 (迭代后)
        R = np.dot(np.dot(G,C),np.transpose(G)) # Rt = G·Ct−1·GT + Wt: # C 为t-1时刻隐变量的方差
        
        R[np.reshape(itrend,[-1,1]),itrend] = R[np.reshape(itrend,[-1,1]),itrend]/deltrend # 关联隐变量的矩阵
        R[np.reshape(iregn,[-1,1]),iregn] = R[np.reshape(iregn,[-1,1]),iregn]/delregn # 隐变量自相关的矩阵
        R[np.reshape(iseas,[-1,1]),iseas] = R[np.reshape(iseas,[-1,1]),iseas]/delseas # 周期效应的矩阵
        
        nu = delvar*nu # IG(nu/2,S*nu/2) Assuming the variation of observational error follows an inverse-gamma (IG) distribution
        
        F[iregn,0] = X[t,] # 关联隐变量的矩阵,iregn = np.arange(ntrend,ntrend+nregn), 2,3 两个位置为NDVI和降水 

        A = np.dot(R,F) 
        Q = np.squeeze(np.dot(np.transpose(F),A)+S)
        A = A/Q
        f = np.squeeze(np.dot(np.transpose(F),a))
        
        y = Y[t]
        
        if ~np.isnan(y): # 如果y不是空值
            
            e = y-f # et = yt − ft # 误差error
            ac = (nu+np.power(e,2)/Q)/(nu+1)
            rQ = np.sqrt(Q)
            mlik = tdstr.pdf(e/rQ,nu)/rQ
            
            m = a+A*e # 影响效应: t时刻隐变量的均值 (迭代后); mt = at + At et; 
            C = ac*(R-np.dot(A,np.transpose(A))*Q) # Ct = Rt − qt At AT
            nu = nu+1
            S = ac*S
            
            # About "S = ac*S" (using the notations in Liu et al. (2019)): 
            # s_t = d_t/n_t = (d_{t-1}+e_t^2/(q_t^{**}/s_t))/n_t = s_{t-1} * (n_{t-1}+e_t^2/(q_t^{**})/n_t = ac * s_{t-1}
        else: # 如果y是空值
            m = a; C = R;
            if t<T-1:
                X[t+1,0] = f
            mlik = np.nan
            
        sm = np.concatenate((sm,m),axis=1) # mean of autocorrelation 
        sC = np.concatenate((sC,np.reshape(C,[C.shape[0],C.shape[1],1])),axis=2) # variance of autocorrelation
        snu = np.concatenate((snu,[nu]),axis=0) # degree of freedom
        sS = np.concatenate((sS,[S]),axis=0)
        slik = np.concatenate((slik,[mlik]),axis=0)
        
    return {'sm':sm, 'sC':sC ,'snu':snu,'slik':slik} 

def computeAnormaly(CLM,AvgCLM,date0):
    deltaT = timedelta(days=16)
    anCLM = np.zeros([1,CLM.shape[1]])
    for i in range(CLM.shape[0]): #对于每一条climate数据: 376条
        st = date0+deltaT*(i); st = st.timetuple().tm_yday
        et = date0+deltaT*(i+1); et = et.timetuple().tm_yday               
        if et<st: # 跨年度情况
            window = np.concatenate((np.arange(st,365),np.arange(0,et)))
        else: # 在一年内
            window = np.arange(st,et)
        window[window==365] = 0  # leap year
        
        # 按行合并;合并了当期16天气象数据与年均值的差
        anCLM = np.concatenate((anCLM,np.reshape(CLM[i,:]- np.mean(AvgCLM[window,:],axis = 0),[1,CLM.shape[1]])),axis=0)
    return anCLM[1:,:]

def Index_low(nn,date0,percentile):
    intervel = 16
    date0_num = date0.toordinal()
    dd = np.arange(date0,date0+timedelta(days=intervel)*len(nn),timedelta(days=intervel))
    dd_num = np.arange(date0_num,date0_num+intervel*(len(nn)),intervel)
    idq = [i for i in range(len(nn)) if np.isfinite(nn[i])] 
    tt1_num = np.arange(dd_num[idq[0]],dd_num[idq[-1]],1)
    f_itp = interp1d(dd_num[idq], nn[idq],kind = 'linear')
    nn_itp = f_itp(tt1_num)
    
    yday = np.array([date.fromordinal(tt1_num[i]).timetuple().tm_yday for i in range(len(tt1_num))])
    
    ndvi_mean = np.array([np.mean(nn_itp[yday==i]) for i in range(1,366)])
    ndvi_std = np.array([np.std(nn_itp[yday==i]) for i in range(1,366)])
    if len(ndvi_mean)==365:
        ndvi_mean = np.concatenate((ndvi_mean,[ndvi_mean[-1]]),axis = 0)
        ndvi_std = np.concatenate((ndvi_std,[ndvi_std[-1]]),axis = 0)
    
    tt2 = np.arange(dd[0],dd[-1],timedelta(days=1))
    tt2_num = np.arange(dd_num[0],dd_num[-1],1)
    yday2 = np.array([date.fromordinal(tt2_num[i]).timetuple().tm_yday for i in range(len(tt2_num))])
    nv = norm.ppf(1-(1-percentile)/2)
    lowboundary = np.array([ndvi_mean[yday2[i]-1]-nv*ndvi_std[yday2[i]-1] for i in range(len(tt2))])
    
    index_low = [i for i in range(len(dd)) if (~np.isnan(nn[i])) and (nn[i]<lowboundary[tt2_num==dd_num[i]])] 
    return index_low

def PlotEWS(N,date0,sm,sC,snu):
    # thresholds for identification of abnormally high autocorrelation (EWS)  
    quantile1 = 0.90
    quantile2 = 0.70
    
    steps = [date0+relativedelta(days=16*i) for i in range(len(N))]
    lown = Index_low(N,date0,0.8)
    lown_continuous = []
    for i in range(len(lown)):
        tile = [j for j in lown if (j<=lown[i] and j>=lown[i]-5)] 
        if len(tile)>2: 
            #NDVI being abnormally low fro more than half of the time within 3 mon
            lown_continuous = np.concatenate([lown_continuous,[lown[i]]])
            lown_continuous = np.array(lown_continuous).astype(int)
    tmp = np.array([steps[i] for i in lown_continuous])
    diebackdate = tmp[0]
    
    steps = np.array(steps)
    
    xpos = datetime(1996,1,1)
    xtick = [datetime(2000,1,1)+relativedelta(years=2*i) for i in range(0,9)]
    
    
    plt.figure(figsize=(7, 8))
    ax1 = plt.subplot(211)
    
    xlim = [datetime(1999,1,1),datetime(2016,7,1)]
    ylim = [0.3,0.85]
    ax1.plot(steps,N,'o-k',markersize=5,label='NDVI')
    ax1.plot(steps[lown_continuous],N[lown_continuous],'or',label='ALN')
    ax1.axvspan(datetime(2007,1,1), datetime(2010,1,1), color='brown', alpha=0.1, lw=0)
    ax1.axvspan(datetime(2011,1,1), datetime(2016,1,1), color='brown', alpha=0.1, lw=0)
    
    xtick = [datetime(2000,1,1)+relativedelta(years=2*i) for i in range(0,9)]
    ax1.set_xticks(xtick,('00','02','04','06','08','10','12','14','16'))
    ax1.set_ylim(ylim)
    ax1.set_xlim(xlim)
    ax1.set_ylabel('NDVI')
    ax1.legend(loc = 'lower left',ncol=2)
    ax1.text(xpos, 0.82, '(a)',fontsize=20)
    ax1.set_xticks(xtick)
    plt.setp(ax1.get_xticklabels(), visible=False)
    
    warmup = 47
    bd = list(map(lambda m,C,nu: m+np.sqrt(C)*tdstr.ppf(quantile1,nu),sm,sC,snu))
    bd2 = list(map(lambda m,C,nu: m+np.sqrt(C)*tdstr.ppf(quantile2,nu),sm,sC,snu))
    
    mbd = np.median(bd2[warmup:])
    ews = np.array([i for i,im in enumerate(sm) if im >mbd])
    ews = ews[ews>warmup]
    ews_continuous = []
    window = int(90/16) # three months
    
    for i in range(len(ews)):
        tile = [j for j in ews if (j<=ews[i] and j>=ews[i]-window)]
        if len(tile)>window-1:
            ews_continuous = np.concatenate([ews_continuous,[ews[i]]])
    ews_continuous = np.array(ews_continuous).astype(int)
    tmp = steps[ews_continuous]
    ewsdate = tmp[tmp>datetime(2012,7,15)][0]
    mortdate = datetime(2015,7,15)
    arrowprops=dict(facecolor='black', shrink=0.05,width=1,headwidth=10)
    ax2 = plt.subplot(212)
    ylim = [-0.5,0.7]
    
    # EWS 
    ax2.plot(steps[1:], sm[1:], lw=2, label='mean')
    ax2.fill_between(steps, 2*sm-bd, bd, facecolor='0.7',label=str(int(quantile1*100))+'% range')
    ax2.fill_between(steps, 2*sm-bd2, bd2, facecolor='0.5',label=str(int(quantile2*100))+'% range')
    ax2.plot([steps[warmup],steps[-1]],[mbd,mbd],'--',color='0.4')
    ax2.plot(steps[ews_continuous],sm[ews_continuous],'^r',markersize=3,label='EWS')
    ax2.axvspan(datetime(2007,1,1), datetime(2010,1,1), color='brown', alpha=0.1, lw=0)
    ax2.axvspan(datetime(2011,1,1), datetime(2016,1,1), color='brown', alpha=0.1, lw=0)
    ax2.set_xlim(xlim)
    ax2.set_xticks(xtick)
    ax2.set_xticklabels(('00','02','04','06','08','10','12','14','16'))
    ax2.set_ylim(ylim)
    ax2.set_ylabel('Autocorrelation')
    ax2.set_xlabel('Year')
    yend = -0.28
    ft = 14
    hshift = relativedelta(months=3)
    ax2.text(mortdate-hshift, 0.05, 'mortality',rotation='vertical',fontsize=ft)
    ax2.text(diebackdate-hshift, 0.00, 'dieback',rotation='vertical',fontsize=ft)
    ax2.text(ewsdate-hshift, -0.12, 'EWS',rotation='vertical',fontsize=ft)
    
    ax2.annotate('', xy=(mortdate, ylim[0]), 
                xytext=(mortdate, yend),arrowprops=arrowprops)
    ax2.annotate('', xy=(diebackdate, ylim[0]), 
                xytext=(diebackdate, yend),arrowprops=arrowprops)
    ax2.annotate('', xy=(ewsdate, ylim[0]), 
                xytext=(ewsdate, yend),arrowprops=arrowprops)
    ax2.legend(loc=9,ncol=2)
    ax2.text(xpos, 0.63, '(b)',fontsize=20)

def calDrift(ts,window):
    qv = (np.nanmax(ts)-np.nanmin(ts))/20
    delta = (np.nanmax(ts)-np.nanmin(ts))/10
    trd_diffusion = (np.nanmax(ts)-np.nanmin(ts))/20
    drift = np.zeros([len(ts)-window-1,1])
    
    for i in range(drift.shape[0]):
        subn = ts[i:i+window]
        DX = subn[1:]-subn[:-1]
        X1 = subn[:-1]
        xq = np.arange(np.nanmin(ts)-0.1*abs(np.nanmin(ts)),np.nanmax(ts)+0.1*abs(np.nanmax(ts)),qv)
        mx = np.zeros(xq.shape)+np.nan
        mx2 = np.zeros(xq.shape)+np.nan
        for j in range(len(xq)):
            tmp = DX[np.abs(X1-xq[j])<delta]
            if len(tmp)>0:
                mx[j] = np.nanmedian(tmp)
                mx2[j] = np.nanmedian(tmp**2)
        idx = [j for j,x in enumerate(mx2) if x<trd_diffusion]
        idx = [j for j in idx if np.isfinite(mx[j])] # remove nan
        if np.size(idx):            
            drift[i] = 1+np.polyfit(xq[idx],mx[idx],1)[0] # slope of <D(x)_1> v.s. x
        else:
            drift[i] = 0
    return drift

def calEmprAC(nn,window):
    empr_ac = np.zeros([len(nn)-window-1,1])
    for i in range(len(empr_ac)):
        t1 = nn[i:i+window]
        t2 = nn[i+1:i+window+1]
        idx = [j for j in range(len(t1)) if np.isfinite(t1[j]+t2[j])]
        if np.size(idx):
            empr_ac[i] = np.corrcoef(t1[idx],t2[idx])[0,1]
        else:
            empr_ac[i] = 0
    return empr_ac

def calEmprVar(nn,window):
    return np.asarray([np.nanstd(nn[i:i+window]) for i in range(len(nn)-window)])

def plotThFig(FF,X,meanX):
    warmup = int(24*1.5)
    quantile = 0.90
    vid = 2

    sm = FF.get('sm')[vid,:]
    sC = np.sqrt(FF.get('sC')[vid,vid,:]) # std
    snu = FF.get('snu')
    bd = list(map(lambda m,C,nu: m+C*tdstr.ppf(quantile,nu),
                      sm,sC,snu))
    mbd = np.median(bd[warmup:])
    ews = np.array([i for i,im in enumerate(sm) if im >mbd])
    ews = ews[ews>warmup]
    ews_continuous = []
    for i in range(len(ews)): # if ews lasts > 3 mon
        tile = [j for j in ews if (j<=ews[i] and j>=ews[i]-5)]
        if len(tile)>4:
            ews_continuous = np.concatenate([ews_continuous,[ews[i]]])
    ews_continuous = np.array(ews_continuous).astype(int)

    xpos = -220
    xlim = [-0.05*len(X),len(X)*1.05]
    plt.figure(figsize=(8, 8))
    ax1 = plt.subplot(311)
    ax1.plot(X+meanX,'-k')
    ax1.set_ylim([0.05,1.05])
    ax1.set_xlim(xlim)
    ax1.set_ylabel('yt')
    ax1.text(xpos, 0.95, '(a)',fontsize=20)
    
    steps = np.arange(len(sm))
    ax2 = plt.subplot(3,1,2)
    ax2.plot(steps,sm,lw=2, label='mean')
    ax2.fill_between(steps,2*sm-bd, bd, facecolor='0.7',label=str(int(quantile*100))+'% range')
    ax2.plot([warmup,len(sm)],[mbd,mbd],'--',color='0.4')
    ax2.plot(steps[ews_continuous],sm[ews_continuous],'^r',markersize=3,label='EWS')
    
    ax2.set_ylim([-0.2,1.2])
    ax2.set_xlim(xlim)
    ax2.set_ylabel('DLM autocorr')
    ax2.legend(loc=9,ncol=3)
    ax2.text(xpos, 1.1, '(b)',fontsize=20)
    
    window = 50
    emprAc = calEmprAC(X,window)
    emprVar = calEmprVar(X,window)
    Drift = calDrift(X,window)
    
    ax3 = plt.subplot(3,1,3)
    ax4 = ax3.twinx()
    nv = norm.ppf(0.8)
    l1 = ax3.plot(steps[window+1:],emprAc,label='autocorr')
    ax3.plot([steps[window],steps[-1]],np.nanmean(emprAc)+nv*np.nanstd(emprAc)*np.array([1,1]),'--',color=l1[0].get_color())
    l2 = ax3.plot(steps[window+1:],Drift,label='drift')
    ax3.plot([steps[window],steps[-1]],np.nanmean(Drift)+nv*np.nanstd(Drift)*np.array([1,1]),'--',color=l2[0].get_color())
    
    ax3.set_xlabel('Time step')
    ax3.set_ylabel('autocorr/drift')
    ax3.set_xlim(xlim)
    ax3.set_ylim([-0.3,1.05])
    l3 = ax4.plot(steps[window:],emprVar,'-g',label='std')
    ax4.plot([steps[window],steps[-1]],np.nanmean(emprVar)+nv*np.nanstd(emprVar)*np.array([1,1]),'--',color=l3[0].get_color())
    ax4.set_ylabel('std')
    ax4.set_ylim([0.03,0.22])
    
    leg3 = ax3.legend(bbox_to_anchor=(0.48, -0.28),ncol=2,)
    leg4 = ax4.legend(bbox_to_anchor=(0.7, -0.28))
    leg3.get_frame().set_linewidth(0.0)
    leg4.get_frame().set_linewidth(0.0)
    ax3.text(xpos, 0.95, '(c)',fontsize=20)
    
def plotThFig2(FF,X,meanX):
    warmup = int(24*1.5)
    quantile = 0.90

    xpos = -220
    xlim = [-0.05*len(X),len(X)*1.05]
    plt.figure(figsize=(8, 8))
    ax1 = plt.subplot(311)
    ax1.plot(X+meanX,'-k')
    ax1.set_ylim([0.05,1.05])
    ax1.set_xlim(xlim)
    ax1.set_ylabel('yt')
    ax1.text(xpos, 0.95, '(a)',fontsize=20)
    
    vid = 0
    sm = FF.get('sm')[vid,:]
    sC = np.sqrt(FF.get('sC')[vid,vid,:]) # std
    snu = FF.get('snu')
    bd = list(map(lambda m,C,nu: m+C*tdstr.ppf(quantile,nu),
                      sm,sC,snu))
    steps = np.arange(len(sm))
    ax2 = plt.subplot(3,1,2)
    ax2.plot(steps,sm,lw=2, label='mean')
    ax2.fill_between(steps,2*sm-bd, bd, facecolor='0.7',label=str(int(quantile*100))+'% range')
    ax2.set_ylim([-0.22,0.22])
    ax2.set_xlim(xlim)
    ax2.set_ylabel('DLM local mean')
    ax2.legend(loc=1,ncol=1)
    ax2.text(xpos, 0.21, '(b)',fontsize=20)
    
    
    vid = 2
    sm = FF.get('sm')[vid,:]
    sC = np.sqrt(FF.get('sC')[vid,vid,:]) # std
    snu = FF.get('snu')
    bd = list(map(lambda m,C,nu: m+C*tdstr.ppf(quantile,nu),
                      sm,sC,snu))
    mbd = np.median(bd[warmup:])
    ews = np.array([i for i,im in enumerate(sm) if im >mbd])
    ews = ews[ews>warmup]
    ews_continuous = []
    for i in range(len(ews)): # if ews lasts > 3 mon
        tile = [j for j in ews if (j<=ews[i] and j>=ews[i]-5)]
        if len(tile)>4:
            ews_continuous = np.concatenate([ews_continuous,[ews[i]]])
    ews_continuous = np.array(ews_continuous).astype(int)
    
    ax3 = plt.subplot(3,1,3)
    ax3.plot(steps,sm,lw=2, label='mean')
    ax3.fill_between(steps,2*sm-bd, bd, facecolor='0.7',label=str(int(quantile*100))+'% range')
    ax3.plot([warmup,len(sm)],[mbd,mbd],'--',color='0.4')
    ax3.plot(steps[ews_continuous],sm[ews_continuous],'^r',markersize=3,label='EWS')
    ax3.set_ylim([-0.2,1.2])
    ax3.set_xlim(xlim)
    ax3.set_ylabel('DLM autocorr')
    ax3.legend(loc=9,ncol=3)
    ax3.text(xpos, 1.1, '(c)',fontsize=20)
    ax3.set_xlabel('Time step')
    
