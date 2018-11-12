#!/usr/bin/env /usr/local/bin/node

/* jshint esversion: 6 */

/*
 * <bitbar.title>Track Brazilian Post Office (Correios) packages</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>Carlos E. Torres</bitbar.author>
 * <bitbar.author.github>cetorres</bitbar.author.github>
 * <bitbar.desc>EN: Track Brazilian Post Office (Correios) packages. PT: Rastreie encomendas dos Correios.</bitbar.desc>
 * <bitbar.abouturl>https://github.com/cetorres/bitbar-track-correios-packages</bitbar.abouturl>
 * <bitbar.image>https://raw.githubusercontent.com/cetorres/bitbar-track-correios-packages/master/screenshot.png</bitbar.image>
 * <bitbar.dependencies>node</bitbar.dependencies>
 */

// Enter the track code here.
// Informe o código de rastreio aqui.
var code = ""; // Ex.: AA123456789BR

// Don't change anything else below this line.
// Não altere mais nada a partir desta linha.
const fs = require('fs');
const https = require('https');
var url = "https://wsmobile.correios.com.br/service/rest/rastro/rastroMobile/FALECONOSC/7KMJ6FISA4/L/T/" + code + "/101";
var logFile = ".track-correios-package.log";

var getLogData = function(callback) {
    fs.readFile(__dirname + '/' + logFile, (e, data) => {
        if (e) {
            callback(null);
        }
        callback(data);
    });
};

var saveLogData = function(data) {
    fs.writeFile(__dirname + '/' + logFile, data, function(err) {
        if (err) {
            console.log(err);
        }
    });
};

var refreshMenu = function() {
    console.log("---");
    console.log("Atualizar | refresh=true");
};

var aboutMenu = function() {
    console.log('---');
    console.log('by Carlos E. Torres | href=http://cetorres.com');
};

const toTitleCase = (phrase) => {    
    return phrase ? phrase
        .toLowerCase()
        .split(' ')
        .map(word => word.charAt(0).toUpperCase() + word.slice(1))
        .join(' ') : "";
};

if (!fs.existsSync(__dirname + '/' + logFile)) {
    saveLogData(0);
}

https.get(url, (res) => {

    res.setEncoding('utf-8');
    var responseString = '';

    res.on('data', function(data) {
        responseString += data;
    });

    res.on('end', function() {       
        var responseObject = JSON.parse(responseString);
        var nomeObjeto = responseObject.objeto[0].nome != undefined ? responseObject.objeto[0].nome : "OBJETO";
        var eventos = responseObject.objeto[0].evento;
        var eventosTotal = eventos != undefined ? eventos.length : 0;

        if (nomeObjeto != "OBJETO" && eventosTotal == 0) {
            console.log("📦 0 | color=yellow");
            console.log("---");
            console.log(nomeObjeto + "\n");
            console.log("Código: " + code);
            console.log("---");
            console.log("Sem eventos no momento.");      
            
            refreshMenu();
            aboutMenu(); 
        }
        else if (nomeObjeto == "OBJETO" && eventosTotal == 0) {
            console.log("📦 0 | color=yellow");
            console.log("---");
            console.log(nomeObjeto + "\n");
            console.log("Código: " + code);
            console.log("---");
            console.log("Objeto não encontrado."); 

            refreshMenu();
            aboutMenu(); 
        }
        else if (nomeObjeto != "" && eventosTotal > 0) {
            var header = eventos.filter(evento => evento.tipo == "PO")[0];            
            
            getLogData((data) => {
                var newEvents = false;
                var eventosLog = data;
            
                if (eventosLog != undefined) {
                    if (eventosLog != eventosTotal) {
                        newEvents = true;
                    }
                }            
                
                saveLogData(eventosTotal);

                var objetoEntregue = eventos.find(e => e.tipo == "BDE") != undefined ? true : false;

                if (objetoEntregue) {
                    console.log("📦 ✓ | color=green");    
                }
                else {
                    console.log("📦 " + eventosTotal + (newEvents ? ' | color=green' : ''));
                }
                console.log("---");            
                console.log(nomeObjeto + "\n");
                console.log("Código: " + code);
                console.log('Postado em: ' + (header == undefined ? eventos[0].dataPostagem : header.postagem.datapostagem));

                eventos.forEach(evento => {                
                    console.log("---");
                    console.log(evento.data + " - " + evento.hora + "\n");                 
                    if (evento.tipo == "RO") {            
                        console.log(evento.descricao.trim() + "\n");                                                        
                        console.log("Para: " + toTitleCase(evento.destino[0].local) + (evento.destino[0].cidade != undefined ? (" em " + toTitleCase(evento.destino[0].cidade)) : ""));
                        console.log("De: " + toTitleCase(evento.unidade.local) + (evento.unidade.cidade != undefined ? (" em " + toTitleCase(evento.unidade.cidade) + "\n") : ""));   
                    } 
                    else if (evento.tipo == "PAR") {
                        console.log(evento.descricao.trim() + "\n");                                                        
                        console.log(toTitleCase(evento.unidade.local));
                    }
                    else {
                        console.log(evento.descricao.trim() + "\n");
                        if (evento.detalhe != undefined) {
                            console.log(evento.detalhe.trim() + "\n");                                
                        }                        
                        if (evento.postagem != undefined) {
                            console.log("Para: " + toTitleCase(evento.postagem.destinatario));
                        }
                        console.log("De: " + toTitleCase(evento.unidade.local) + (evento.unidade.cidade != undefined ? (" em " + toTitleCase(evento.unidade.cidade) + "\n") : "")); 
                    }               
                });

                console.log("---");
                console.log("Ver online | href=" + url); 
                
                refreshMenu();
                aboutMenu(); 
            });                 
            
        }
        
    });

})
.on('error', (e) => {
    console.log('✖︎ | color=red');
    console.log('---');
    console.log('Erro ao ler dados do serviço dos Correios.\n' + e);
    refreshMenu();
    aboutMenu();
});
