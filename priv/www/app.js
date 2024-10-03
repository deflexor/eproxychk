

var SvcTab = {
    g_state: {},
    startTimer: function() {
        self = this
        if(dojo.style(dojo.byId("paneServices"), 'display') === 'block') {
            self.timer = setTimeout(function() { self.updateSvcStatus() }, 5000)
        }
    },
    updateStartStopButton: function(status,b,s,statusStr) {
        var self = this
        if(status === 'on') {
            dojo.attr(s[0], {
                innerHTML: "Работает",
                style: { color: "green" }
            })
            dojo.attr(s[1], { innerHTML: statusStr });
            b.set('label', "Остановить")
            b.set('value', "started")
        }
        else if(status === 'noproc') {
            dojo.attr(s[0], {
                innerHTML: "Остановлен",
                style: { color: "red" }
            })
            dojo.attr(s[1], { innerHTML: "" })
            b.set('label', "Запустить")
            b.set('value', "stopped")
        }
        else {
            dojo.attr(s[0], {
                innerHTML: status,
                style: { color: "blue" }
            })
            b.set('label', "Остановить")
            b.set('value', "started")
        }
        b.set('disabled', false)
    },
    updateSvcStatus: function() {
        self = this
        self.svc.svcInfo().then(function(resp) {
            self.updateCheckerStatus(resp.checker)
            self.updateParserStatus(resp.parser)
            if(resp.checker.status !== 'noproc' || resp.parser.status !== 'noproc') {
                self.startTimer()
            }
        })
    },
    updateCheckerStatus: function(resp) {
        var self = this
        var b = dijit.byId("btnStartChecker")
        // call info
        // checker speed
        var epoch = (new Date()).getTime()
        var speed = 0
        if(self.g_state.last_checker_time > 0 && self.g_state.last_checker_srvs > 0) {
            self.g_state.total_checker_time += epoch - self.g_state.last_checker_time
            self.g_state.total_checker_srvs += self.g_state.last_checker_srvs - resp.srv_update
            speed = dojo.number.round(self.g_state.total_checker_srvs / self.g_state.total_checker_time * 1000, 2)
        }
        self.g_state.last_checker_time = epoch
        self.g_state.last_checker_srvs = resp.srv_update

        // update status text and start/stop button
        var s = dojo.query(".checkerInfo")
        //console.log(resp.status)
        var statusStr = "Кол-во процессов: <b>" + resp.npids + "</b>, "
            + "Размер очереди: <b>" + resp.batch + "</b>, "
            + "Осталось обновить: <b>" + resp.srv_update + "</b> из <b>" + resp.srv_total + "</b>"
        if(speed > 0) statusStr += " Скорость: <b>" + speed + " серв/сек</b>"
        
        self.updateStartStopButton(resp.status, b, s, statusStr)
    },
    updateParserStatus: function(resp) {
        var self = this
        var b = dijit.byId("btnStartParser")
        // update status text and start/stop button
        var s = dojo.query(".parserInfo")
        var statusStr = "Найдено серверов: <b>" + resp.found + "</b>, "
            + "Добавлено новых: <b>" + resp.saved + "</b>"
        
        self.updateStartStopButton(resp.status, b, s, statusStr)
    },
    startStopParser: function(evt){
        var self = this
        var b = dijit.byId("btnStartParser")
        b.set('disabled', true)
        var cmd = b.get('value') != 'started' ? this.svc.startParser : this.svc.stopParser;
        cmd().then(function(resp){
            if(resp != 'ok') {
                var s = b.get('value') != 'started' ? 'запуске' : 'остановке';
                dojo.publish("toaster",[{ message: "Ошибка при "+s+" парсера!", type: "fatal"}])
            }
            self.updateSvcStatus()
        })
    },
    startStopChecker: function(evt){
        var self = this
        var b = dijit.byId("btnStartChecker")
        b.set('disabled', true)
        var cmd = b.get('value') != 'started' ?
            self.svc.startChecker : self.svc.stopChecker;
        cmd().then(function(resp){
            if(resp != 'ok') {
                var s = b.get('value') != 'started' ? 'запуске' : 'остановке';
                dojo.publish("toaster",[{ message: "Ошибка при "+s+" чекера!", type: "fatal"}])
            }
            else {
                // for speed
                self.g_state = { last_checker_time: 0, total_checker_time: 0, total_checker_srvs: 0, last_checker_srvs: 0 };
            }
            self.updateSvcStatus()
        })
    },
    setChecker: function(evt){
        self = this
        var b = dijit.byId("btnSetChecker")
        var v = dijit.byId("txtCheckerBatch").get("value")
        if(v > 50) {
            dojo.publish("toaster",[{ message: "не больше 50", type: "fatal"}])
            return
        }
        b.set('disabled', true)
        self.svc.checkerSetBatch(v).then(function(resp){
            if(resp == 'ok') {
                self.updateSvcStatus()
            }
            else {
                var msg = resp.error ? "Неверное значение параметра" : "Ошибка при обновлении параметров чекера!"
                dojo.publish("toaster",[{ message: msg, type: "fatal"}])
            }
            b.set("disabled", false)
        })
    },
    getSettings: function() {
        this.svc.getSetting({ key: "checker_batch_size" }).then(function(resp) {
            if(! isNaN(resp)) dijit.byId("txtCheckerBatch").set("value", resp)
        })
    },
    constructor: function(svc) {
        var self = this
        self.svc = svc

        dijit.byId("btnStartChecker").on('Click',
                                         function(e) { self.startStopChecker(e) })
        dijit.byId("btnStartParser").on('Click',
                                         function(e) { self.startStopParser(e) })
        dijit.byId("btnSetChecker").on('Click',
                                       function(e) { self.setChecker(e) })
        dijit.byId("paneServices").on('Show',function(e) {
            self.getSettings()
            self.updateSvcStatus()
            self.startTimer()
        })

        //dijit.byId('tabContainer').forward()
        //dijit.byId('tabContainer').forward()
        
    }
}

var SettingsTab = {
    saveSettings: function(e) {
        var self = this
        var frm = dijit.byId('settingsForm')
        var frm0 = dojo.byId('settingsForm')
        var btn = dijit.byId("btnSaveSettings")
        dojo.stopEvent(e)
	if(frm.validate()) {
            btn.set('disabled', true)
            self.svc.saveSettings({ obj: dojo.formToObject(frm0) }).then(function(resp) {
                    dojo.publish("toaster",[{ message: "Данные сохранены"}])
                    btn.set('disabled', false)
            })
        }
    },
    getSettings: function() {
        var self = this
        var frm = dijit.byId('settingsForm')
        var fields = dojo.forEach(frm.getChildren(), function(item, i) {
            var fld = item.get('name')
            self.svc.getSetting({ key: fld }).then(function(resp) {
                item.set("value", resp)
                if(resp == '' && fld == 'my_ip') {
                    self.svc.myip().then(function(ip) {
                        item.set("placeholder", ip)
                    })
                }
            })
        })
    },
    constructor: function(svc) {
        var self = this
        self.svc = svc

        dijit.byId("paneSettings").on('Show',function(e) {
            self.getSettings()
        })
        dijit.byId("paneSettings").on('Hide',function(e) {
        })
        dijit.byId("settingsForm").on('Submit',function(e) { self.saveSettings(e) })
    }
}
