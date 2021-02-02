const Elm = require('./elm.min.js').Elm;
const path = require('path');
const fs = require('fs');



basePath = process.argv[0]
args = process.argv.slice(2)

const app = Elm.Worker.init();


// load files

const inputDir = path.join(__dirname, 'tests/TestInputs');

const outputDir = path.join(__dirname, 'tests/GeneratedTests');
if(!fs.existsSync(outputDir))
{
    console.log("Creating outputdir : " +outputDir);
    fs.mkdirSync(outputDir);
}
fs.readdir(inputDir, function (err, files) {
    //handling error
    if (err) {
        return console.log('Unable to scan directory: ' + err);
    }
    //listing all files using forEach
    files.forEach(function (file) {
        // Do whatever you want to do with the file
        fs.readFile(path.join(inputDir,file), function(fileErr,data){
            if(fileErr) {
                return console.log('Unable to load file: ' + fileErr);
            }
            data = {"fileName" : file, "content":data.toString()}
            app.ports.stdin.send(data)
        });

    });
});


//Elm output
app.ports.stdout.subscribe((message ) =>{
    //TODO write to the file

    if(typeof(message) === "string")
    {
        process.stdout.write(message.toString())
        return;
    }


    fs.writeFile(path.join(outputDir,message.fileName), message.content, (err) => {
        if (err)
        {
            console.log(err);
            return;
        }
    });
    console.log("Wrote testfile '" + message.fileName + "' to " + outputDir);



});
