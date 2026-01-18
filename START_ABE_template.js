// Autoren: Albrecht, J.(Umsetzung Alt- und Totholzkonzept); Neumann, J. (waldbauliche Implementierung der Waldentwicklungstypen-Richtlinie BW)
// Datum finaler Version: Januar 2026






// ================================================================================================
// LIBRARY LOADING & INITIALIZATION
// ================================================================================================

// Globals.include(Globals.path('C:/Users/Jakob Albrecht/Documents/Modelling/iLand/southern_Black_Forest/abe/abe-lib/ABE-library.js'));
Globals.include(Globals.path('C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand/iLand_southern_Black_Forest/Management/JavaScript/abe-lib/ABE-library.js'));

// we need the js stand object 
lib.initAllStands();
lib.loglevel = 2;

// // general function that is executed at the end of each simulated year
// function onYearEnd() {
//     if (Globals.year % 10 == 0) {
//         BarkBeetle.gridToFile('sumVolume', 'output/disturbances/barkbeetle/' + Globals.setting('user.Scenario') + '_Year' + Globals.year + '.asc')
//         Wind.gridToFile('sumVolume', 'output/disturbances/wind/' + Globals.setting('user.Scenario') + '_Year' + Globals.year + '.asc')
//     }    
// }

/** Set up some STPs
 
*/

/* No Mgmt
Just one harvest activity where nothing happens
*/

// const NoMgmt = lib.harvest.noHarvest();
const NoMgmt = lib.harvest.noManagement();
lib.createSTP('no_mgmt', NoMgmt); // no_mgmt


// ----------------------------------------- No species choice -------------------------------------------------

// ----------------------------------------------- Speed --------------------------------------------------------
// variation in Speed - first slow management
var SpeedFactor = 1.0; // 1 is regular, 0.8: 20% slower








// ================================================================================================
// HABITAT TREES, NO MANAGEMENT (author: Albrecht, J.)
// ================================================================================================

/* Habitat tree selection
* This STP selects habitat trees (trees with TreeNoHarvest flag) to ensure a minimum of 5 habitat trees per hectare.
* It runs every 10 years, starting at year 20.
* Selection criteria:
* - Only trees with dbh > 30 cm can be selected as habitat trees.
* - largest trees (by dbh) 
* ToDO: 
    - Habitatbaumgruppen (750 m2 27.5*27.5 m) anlegen
    - How to deal with spacing between different stands? This is especially a problem with small stands 
    - Scheduling: Alternative
*/

const HBG_Init = lib.repeater({
    // schedule: { signal: 'ready_for_harvest' },
    id: 'HBG_Init',
    schedule: {min: 0, opt: 1, max: 1, absolute: true, force: true}, // run only once at the beginning of the simulation
    count: 1, 
    interval: 1,
    block: false, 
    // parameter: function() {
    //     // Example: Return the current stand's basal area as the parameter.
    //     return stand.basalArea();
    // }
    signal: 'HBG_SelectPatches' // signal to start the habitat tree selection process
});

const HBG_Repeater = lib.repeater({
    // schedule: { signal: 'ready_for_harvest' },
    id: 'HBG_Repeater',
    schedule: { 
    min: 85/SpeedFactor, 
    opt: 85/SpeedFactor, 
    max: 200/SpeedFactor, 
    force: true 
    }, 
    // schedule: {
    // minRel: 30/SpeedFactor, 
    // optRel: 30/SpeedFactor, 
    // maxRel: 40/SpeedFactor, 
    // force: true 
    // },
    count: 1, 
    interval: 1,
    // parameter: function() {
    //     // Example: Return the current stand's basal area as the parameter.
    //     return stand.basalArea();
    // }
    signal: 'HBG_SelectPatches' // signal to start the habitat tree selection process
});

const HBG_SelectPatches = lib.selectOptimalPatchesRepeat({ 
    id: 'HBG_SelectPatches',
    schedule: { signal: 'HBG_SelectPatches' },
    N: 2,
    patchId: 100, 
    patchsize: 3, 
    spacing: 3,
    criterium: 'max_species',
    sendSignal: 'HBG_PatchesSelected',
});

const HBG_SelectPatches_f = lib.selectOptimalPatchesRepeat({ 
    id: 'HBG_SelectPatches_f',
    // schedule: { signal: ['HBG_SelectPatches', 'selective_thinning_remove', 'PatchesSelected'] },
    schedule: { signal: 'HBG_SelectPatches' },
    // schedule: { signal: 'HBG_SelectPatches' }, alternative: use HBG_Repeater for emmitting the signal
    // schedule: {opt: 10, absolute: true}, // absolute: true -> only run once
    // schedule: {
    //     minRel: 0.05/SpeedFactor, 
    //     optRel: 0.10/SpeedFactor, 
    //     maxRel: 0.10/SpeedFactor, 
    //     force: true 
    // },
    // times: Math.round(1*SpeedFactor), // number of times the selection is done
    // interval: Math.round(10/SpeedFactor), // interval in years
    N: 2,
    patchId: 100, // patchId for habitat tree patches
    patchsize: 3, 
    spacing: 3, // AuT: abstand zwischen HBG sollte im Durchschnitt 170 m betragen -> 17 
    criterium: 'max_species', 
    // customPrefFunc: '',
    sendSignal: 'HBG_PatchesSelected',
});


const HBG_Marking = {
    id: 'HBG_Marking',
    type: 'general',
    schedule: { signal: 'HBG_PatchesSelected' }, 
    action: function() {
        // Ensure stand.obj is initialized before calling activityLog
        lib.initStandObj();
        
        lib.activityLog('HBG_Marking')
        stand.simulate = false;

        // Count current habitat trees (trees with TreeNoHarvest flag)
        stand.trees.loadAll();
        var currentHt = stand.trees.sum('marknoharvest');
        var currentHtPerHa = currentHt / stand.area;
        var targetHtPerHa = 5; // Target: 5 habitat trees per hectare
        var targetHt = targetHtPerHa * stand.area;
        
        fmengine.log("Current habitat trees: " + currentHt + " (" + currentHtPerHa.toFixed(1) + " per ha)");
        
        // Loop over all patches in stand.patches.list
        for (let i = 0; i < stand.patches.list.length; i++) {
            const patch = stand.patches.list[i];
            const patchId = patch.id;
            
            // Only select new habitat trees if we have less than 5 per hectare
            if (currentHt < targetHt) {
                // Filter to only trees in this specific patch with minimum dbh
                var treesInPatch = stand.trees.load('patch = ' + patchId + ' and dbh > 15');
                fmengine.log("Trees found in patch " + patchId + " with dbh>15: " + treesInPatch);

                if (treesInPatch > 0) {
                    // Calculate how many trees we need to mark for this patch
                    var treesNeededForPatch = Math.ceil(targetHt - currentHt);
                    treesNeededForPatch = Math.min(treesNeededForPatch, treesInPatch); // Don't exceed available trees
                    
                    if (treesNeededForPatch > 0) {
                        // Sort by dbh (largest first) and select the best trees
                        // stand.trees.filter('species<>piab and species<>psme'); // Possibility to exclude specific species if needed Attention: can lead to very few habitat trees per stand 
                        stand.trees.sort('-dbh'); // - = descending order
                        stand.trees.filter('incsum(1) <= ' + treesNeededForPatch);
                        

                        // Mark selected trees as habitat trees
                        stand.trees.setFlag(Tree.TreeNoHarvest);
                        // fmengine.log("Marked " + stand.trees.count + " trees as habitat trees in patch " + patchId);
                        currentHt += stand.trees.count; // Update current habitat trees count
                        currentHtPerHa = currentHt / stand.area;
                        fmengine.log("Updated current habitat trees: " + currentHt + " (" + currentHtPerHa.toFixed(1) + " per ha)");
                
                    }
                } else {
                    fmengine.log("No suitable trees found in patch " + patchId + " (dbh > 15)");
                }
            } else {
                fmengine.log("Sufficient habitat trees already present (" + currentHt/stand.area + " per ha)");
                break; // Exit the loop early since we have enough habitat trees
            }
        }; 
        // send signal to start tDBH harvest
        stand.stp.signal('HBG_Marking_done'); // Emit a signal when marking is done
        lib.dbg('HBG_Marking created')
    }, 
    
};






// ================================================================================================
// WET MANAGEMENT SYSTEMS (author: Neumann, J.)
// ================================================================================================

// ------------------------------------------------------------------------------------------------
// WET_bg - Buchen-Mischwald Geringe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const bg_Tending = lib.thinning.tending({
    id: 'bg_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(40/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor),
    interval: Math.round(4/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false
});

// dynamisierte Auslese-Df
const bg_Thinning = lib.thinning.selectiveThinning({
    id: 'bg_Thinning',
    schedule: {
        min: Math.round(35/SpeedFactor), 
        opt: Math.round(35/SpeedFactor), 
        max: Math.round(75/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic', 
    nTrees: 70, 
    nCompetitors:11*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,5,5,8,9,9,11], // Vorratspflege! -> kein EIngriff ergänzt, da ZD-Ernte beginnt; 2x 5 zu Beginn ergänzt wegen starkem Vorratsanstieg
    times: Math.round(8*SpeedFactor),
    decayFactor: 1.35,
    preferenceFunction: 'height / (stress * 0.8 + 1)',
    speciesSelectivity: {
        "fasy":1.3, 
        "quro":1,
        "qupe":1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.1, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.1, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":0.2, // bei 1.0 wurde relativ häufig Ta ausgezeichnet
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Zieldurchmesserernte
const bg_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'bg_tDBH_harvest',
     schedule: {
        min: Math.round(85/SpeedFactor), 
        opt: Math.round(85/SpeedFactor), 
        max: Math.round(200/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true 
    },
    targetDBH: 50/SpeedFactor, 
    times: Math.round(2*SpeedFactor), // WET24: soll nach fortschreitender Nutzung in Femel übergehen
    interval: Math.round(5/SpeedFactor),
    dbhList: { 
        //geringes Risiko
        // LB:
        "fasy":60/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":50/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":80/SpeedFactor, //Standard 50
        "piab":50/SpeedFactor, // geästet 60 (aber i.d.R. Standardsortimente)
        "pisy":60/SpeedFactor, // Standard 45
        "pini":60/SpeedFactor, // Standard 45
        "lade":60/SpeedFactor, // Standard 45         
        "abal":80/SpeedFactor, // Standard 50
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
    // zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    },
    // sendSignal: 'target_DBH_completed'
});  

// Femel
const bg_SelectPatches = lib.selectOptimalPatches({
    id: 'bg_SelectPatches',
    // schedule: {signal: 'target_DBH_completed'},
    schedule: {
        min: Math.round(90/SpeedFactor), 
        opt: Math.round(95/SpeedFactor), 
        max: Math.round(200/SpeedFactor),
        force: true 
    },
    N: Math.round(1*SpeedFactor),
    patchId: 1,
    patchsize: 2, // = 400 m²; damit angepasst an Bartsch et al.: Femel -> Angriffshieb auf bis zu 500 m² Ausdehnung
    spacing: 4, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});

const bg_HarvestPatches = lib.harvest.femel({
    id: 'bg_HarvestPatches',
    schedule: {signal: 'PatchesSelected'}, 
    steps: Math.round(2*SpeedFactor),
    growBy: 1, 
    interval: Math.round(10/SpeedFactor),
    harvestAll: true, // überprüfen, ob bereits etablierte Verjüngung entfernt wird! - ggf. false setzen & finalHarvest activity erstellen
});

// bg_Tending, bg_Thinning, bg_Harvest_tDBH, bg_SelectPatches, bg_HarvestPatches
lib.createSTP('WET_bg', bg_Tending, bg_Thinning, bg_Harvest_tDBH, bg_SelectPatches, bg_HarvestPatches); // WET_bg


// ------------------------------------------------------------------------------------------------
// WET_b - Buchen-Mischwald Mittlere Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const bm_Tending = lib.thinning.tending({
    id: 'bm_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(35/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(4/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false
});

// dynamisierte Auslese-Df
const bm_Thinning = lib.thinning.selectiveThinning({
    id: 'bm_Thinning',
    schedule: {
        min: Math.round(29/SpeedFactor), 
        opt: Math.round(29/SpeedFactor),
        max: Math.round(75/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 70, 
    nCompetitors:11*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,5,5,8,9,9,11], // Vorratspflege! kein zusätzlichen Eingriff ergänzt; entsprechend bg 2x 5 ergänzt
    times: Math.round(8*SpeedFactor),
    decayFactor: 1.35,
    preferenceFunction: 'height / (stress * 0.8 + 1)',
    speciesSelectivity: {
        "fasy":0.9, // etwas geringer als WET_bg, um Mischbaumarten zu begünstigen
        "quro":1.1,
        "qupe":1.1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.1, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.1, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":0.2, // bei 1.0 wurde relativ häufig Ta ausgezeichnet
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Zieldurchmesserernte
const bm_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'bm_tDBH_harvest',
     schedule: {
        min: Math.round(80/SpeedFactor), 
        opt: Math.round(80/SpeedFactor),
        max: Math.round(200/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true 
    },
    targetDBH: 50/SpeedFactor, 
    times: Math.round(2*SpeedFactor), // WET24: soll nach fortschreitender Nutzung in Femel übergehen, bei bm früherer Übergang als bei bg = weniger & kürzer
    interval: Math.round(5/SpeedFactor),
    dbhList: { 
        // mittleres + hohes Risiko
        // LB:
        "fasy":50/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":45/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":50/SpeedFactor, 
        "piab":50/SpeedFactor, 
        "pisy":45/SpeedFactor, 
        "pini":45/SpeedFactor,
        "lade":45/SpeedFactor,         
        "abal":50/SpeedFactor, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
// zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    },
});  

// Femel
const bm_SelectPatches = lib.selectOptimalPatches({
    id: 'bm_SelectPatches',
    // schedule: {signal: 'target_DBH_completed'},
    schedule: {
        min: Math.round(90/SpeedFactor), 
        opt: Math.round(90/SpeedFactor), //  5 Jahre früher als bei WET_bg
        max: Math.round(200/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true 
    },
    N: Math.round(1*SpeedFactor),
    patchId: 1,
    patchsize: 5, // = 2.500 m² -> damit größer als in WET_bg, um Lichtbaumarten zu fördern
    spacing: 4, //geringer als bei WET_bg, da schnelleres Räumen angestrebt wird
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});

const bm_HarvestPatches = lib.harvest.femel({
    id: 'bm_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor),
    growBy: 1, 
    interval: Math.round(10/SpeedFactor),
    harvestAll: true, // überprüfen, ob bereits etablierte Verjüngung entfernt wird! - ggf. false setzen & finalHarvest activity erstellen
});

const bm_clearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

// Pflanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf einer Pflanzung ermittelt
const bm_Pflanzung = lib.planting.dynamic({
    id: 'bm_Pflanzung', 
    schedule: { signal: 'Saplings_cleared' },
    //  schedule: {
    //     min: Math.round(92/SpeedFactor), 
    //     opt: Math.round(92/SpeedFactor), 
    //     max: Math.round(92/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 90 = init Femel; Alter 91 = 1. Femel laut log)
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,
    patches: 'patch=1' // only plant in patches with patchId 1
});

// bg_Tending, bm_Thinning, bm_Harvest_tDBH, bm_SelectPatches, bm_HarvestPatches, bm_clearSaplings, bm_Pflanzung
lib.createSTP('WET_bm', bm_Tending, bm_Thinning, bm_Harvest_tDBH, bm_SelectPatches, bm_HarvestPatches, bm_clearSaplings, bm_Pflanzung);


// ------------------------------------------------------------------------------------------------
// WET_bx - Buchen-Mischwald Hohe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const bx_Tending = lib.thinning.tending({
    id: 'bxTending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(20/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(4/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false,
    // constraint: add top height constraint?
});

// Df
// dynamisierte Auslese-Df
const bx_Thinning = lib.thinning.selectiveThinning({
    id: 'bx_Thinning',
    schedule: {
        min: Math.round(29/SpeedFactor), 
        opt: Math.round(29/SpeedFactor),
        max: Math.round(75/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 70, 
    nCompetitors:11*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,5,5,8,9,9,11], // Vorratspflege! Ein Eingriff nach 11 Jahren ergänzt; entsprechend bg 2x 5 ergänzt
    times: Math.round(8*SpeedFactor),
    decayFactor: 1.35,
    preferenceFunction: 'height / (stress * 1.5 + 1)', // includes stress factor with a weight of 1.5
    speciesSelectivity: {
        "fasy":0.7, // geringer als WET_bg und -_bm, um Mischbaumarten zu begünstigen
        "quro":1.5,
        "qupe":1.5,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.2, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.01, // abnehmende Eignung
        "pisy":1,
        "pini":1.5,
        "lade":1,        
        "abal":0.2, // bei 1.0 wurde relativ häufig Ta ausgezeichnet
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Zieldurchmesserernte
const bx_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'bx_tDBH_harvest',
     schedule: {
        min: Math.round(80/SpeedFactor), 
        opt: Math.round(80/SpeedFactor), // 5 Jahre früher als bei WET_bg
        max: Math.round(85/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true
    },
    targetDBH: 50/SpeedFactor, 
    times: Math.round(1*SpeedFactor), // WET24: mit Schaddynamik Verzichtt auf Zieldurchmesserernte -> deswegen nur 1x
    interval: Math.round(5/SpeedFactor),
    dbhList: { 
        // mittleres + hohes Risiko
        // LB:
        "fasy":50/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":45/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":50/SpeedFactor, 
        "piab":50/SpeedFactor, 
        "pisy":45/SpeedFactor, 
        "pini":45/SpeedFactor,
        "lade":45/SpeedFactor,         
        "abal":50/SpeedFactor, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
// zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    }, 
});  

// Femel
const bx_SelectPatches = lib.selectOptimalPatches({
    id: 'bx_SelectPatches',
    schedule: {
        min: Math.round(85/SpeedFactor), 
        opt: Math.round(90/SpeedFactor), // 10 Jahre früher als bei WET_bg
        max: Math.round(200/SpeedFactor), // ggf. anpassen: Alter > UTZ, da Ernte in jedem Fall durchgeführt werden muss (auch in älteren Beständen)
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 6, // 3.600 m² -> für Ei-Pflanzung
    spacing: 4,  
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});

const bx_HarvestPatches = lib.harvest.femel({
    id: 'bx_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor),
    growBy: 2, // schnelles Rändeln/Räumen
    interval: Math.round(10/SpeedFactor),
    harvestAll: true, // überprüfen, ob bereits etablierte Verjüngung entfernt wird! - ggf. false setzen & finalHarvest activity erstellen
    // finalHarvest: true,
    // preferenceFunction: 'dbh > 15', 
});

const bx_clearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
}};

// Pflanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf einer Pflanzung ermittelt
const bx_Pflanzung = lib.planting.dynamic({
    id: 'bx_Pflanzung', 
    schedule: { signal: 'Saplings_cleared' },
    //  schedule: {
    //     min: Math.round(92/SpeedFactor), 
    //     opt: Math.round(92/SpeedFactor), 
    //     max: Math.round(92/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 90 = init Femel; Alter 91 = 1. Femel laut log)
    patches: 'patch=1', // only plant in patches with patchId 1
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,
});

// lib.createSTP('WET_bx', HBG_SelectPatches, habitatTreeSelection, bx_Tending, bx_Thinning, bx_Harvest_tDBH, bx_PlantPatches, bx_finalHarvest); 
lib.createSTP('WET_bx', bx_Tending, bx_Thinning, bx_Harvest_tDBH, bx_SelectPatches, bx_HarvestPatches, bx_clearSaplings, bx_Pflanzung);


// ------------------------------------------------------------------------------------------------
// WET_e - Eichen-Mischwald Geringe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const eg_Schirm_Tending = lib.thinning.tending({
    id: 'eg_Schirm_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(30/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    }, 
    mode_times: 'dynamic',
    times: Math.round(4*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(5/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false,
});

// Df
// dynamisierte Auslese-Df
const eg_Schirm_Thinning = lib.thinning.selectiveThinning({
    id: 'eg_Schirm_Thinning',
    schedule: {
        min: Math.round(36/SpeedFactor), 
        opt: Math.round(36/SpeedFactor), 
        max: Math.round(165/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 65, 
    nCompetitors:12*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,8,8,12,12,13,13,15,15], // Vorratspflege! 2x 15 ergänzt; 2x 8 ergänzt wegen starkem Vorratsanstieg
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.25,
    preferenceFunction: 'height / (stress * 0.8 + 1)', // includes stress factor with a weight of 0.8
    speciesSelectivity: {
        "fasy": 0.4, // Buche reduziert, um Eichen zu fördern
        "quro": 1.5, // Traubeneiche bevorzugt
        "qupe": 1.5, // Stieleiche bevorzugt
        "acps": 1.2, 
        "acpl": 1.2,
        "acca": 1.2,
        "casa": 1.2,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.0,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
     // NB:
        "psme": 0.1,
        "piab": 0.1,
        "pisy": 0.8,
        "pini": 0.8,
        "lade": 0.8,        
        "abal": 0.2,
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});    

// Schirmschlag für geringe Risikoeinstufung //
// Vorbereitungshieb (Auszug Unter- und Zwischenstand)
const eg_Schirm_Vorbereitungshieb = { type: 'general',
    schedule: {
        min: Math.round(165/SpeedFactor), 
        opt: Math.round(165/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and height<15');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
}};

// Schirmschlag für geringe Risikoeinstufung
const eg_Schirm_Shelterwood = lib.harvest.shelterwood({
    id: 'eg_Schirm_Shelterwood',
    // schedule: { signal : 'Auszug_completed' },
    schedule: {
        min: Math.round(166/SpeedFactor), 
        opt: Math.round(166/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    nTrees: Math.round(60*SpeedFactor), // = Z-Baumzahl in Df
    nCompetitors: Math.round(1000*SpeedFactor), // So hoch, um sicherzustellen, dass nur die Samenbäume verbleiben?
    interval: Math.round(3/SpeedFactor), 
    times: 3, // = 1 Besamungshieb, 2 Lichtungshiebe
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
       // LB:
        "quro": 2.5, // sehr starke Bevorzugung
        "qupe": 2.5,
        "fasy": 0.3, // Buche stark reduziert, um Eichen zu fördern
        "acps": 1.0, 
        "acpl": 1.0,
        "acca": 1.0,
        "casa": 1.0,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.3,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
        // NB:
        "psme": 0.3,
        "piab": 0.1,
        "pisy": 0.6,
        "pini": 0.6,
        "lade": 0.6,
        "abal": 0.3,
        // ohne Ziel-BHD-Angabe in WET2024:
        "qupu": 1.3,
        "soau": 1.0,
        "coav": 1.0
    },
    finalHarvest: true, // um finale Räumung im Anschluss zu ermöglichen
    // constraint: function() {
    // // Lade alle Eichen im Bestand
    // var oakTrees = stand.trees.load('species="quro" or species="qupe"');
    
    // if (oakTrees === 0) {
    //     return false; // Keine Eichen vorhanden
    // }
    
    // // Zähle Eichen mit BHD >= 70 cm
    // var matureOaks = stand.trees.load('(species="quro" or species="qupe") and dbh >= 70');
    
    // // Berechne den Anteil
    // var percentageMature = (matureOaks / oakTrees) * 100;
    
    // // Logging für Debugging
    // fmengine.log("Eichen gesamt: " + oakTrees + ", mit BHD >= 70cm: " + matureOaks + " (" + percentageMature.toFixed(1) + "%)");
    
    // // Rückgabe: true wenn >= 50% der Eichen BHD >= 70 cm haben
    // return percentageMature >= 50;
    // },
    // sendSignal: 'Shelterwood_completed'
});

// eg_Schirm_Tending, eg_Schirm_Thinning, eg_Schirm_Vorbereitungshieb, eg_Schirm_Shelterwood
lib.createSTP('WET_eg_Schirm', eg_Schirm_Tending, eg_Schirm_Thinning,  eg_Schirm_Vorbereitungshieb, eg_Schirm_Shelterwood);


// ------------------------------------------------------------------------------------------------
// WET_e - Eichen-Mischwald Mittlere Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const em_Schirm_Tending = lib.thinning.tending({
    id: 'em_Schirm_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(20/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(4*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(5/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false
});

// Df - dynamisierte Auslese-Df  
const em_Schirm_Thinning = lib.thinning.selectiveThinning({
    id: 'em_Schirm_Thinning',
    schedule: {
        min: Math.round(30/SpeedFactor), 
        opt: Math.round(30/SpeedFactor),
        max: Math.round(150/SpeedFactor),
    },
    mode_times: 'dynamic',
    nTrees: 60, // 40-60 Z-Bäume, 
    nCompetitors: Math.round(14*SpeedFactor),
    useDynamicIntervals: true,
    intervalPattern: [0,8,8,12,12,13,13,15,15], // Vorratspflege! 2x 15 Jahre ergänzt; entsprechend eg 2x 8 ergänzt
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.25, 
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "fasy": 0.2, // Buche reduziert, um Eichen zu fördern
        "quro": 1.5, // Traubeneiche bevorzugt
        "qupe": 1.5, // Stieleiche bevorzugt
        "acps": 1.2, 
        "acpl": 1.2,
        "acca": 1.2,
        "casa": 1.2,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.0,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
     // NB:
        "psme": 0.1,
        "piab": 0.1,
        "pisy": 0.8,
        "pini": 0.8,
        "lade": 0.8,        
        "abal": 0.2,
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Schirmschlag für mittlere Risikoeinstufung //
// Vorbereitungshieb (Auszug Unter- und Zwischenstand)
const em_Schirm_Vorbereitungshieb = { type: 'general',
    schedule: {
        min: Math.round(160/SpeedFactor), 
        opt: Math.round(160/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and height<15');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
}};

// Schirmschlag für mittlere Risikoeinstufung
const em_Schirm_Shelterwood = lib.harvest.shelterwood({
    id: 'em_Schirm_Shelterwood',
    schedule: {
       min: Math.round(161/SpeedFactor), 
        opt: Math.round(161/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    nTrees: Math.round(60*SpeedFactor), // = Z-Baumzahl in Df
    nCompetitors: Math.round(1000*SpeedFactor),
    interval: Math.round(3/SpeedFactor),
    times: 3, // = 1 Besamungshieb, 2 Lichtungshiebe
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "quro": 2.0, // etwas weniger stark bevorzugt als bei geringer Risikoeinstufung
        "qupe": 2.0,
        "fasy": 0.4,
        "acps": 1.0, 
        "acpl": 1.0,
        "acca": 1.0,
        "casa": 1.0,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.3,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
        // NB:
        "psme": 0.3,
        "piab": 0.1,
        "pisy": 0.6,
        "pini": 0.6,
        "lade": 0.6,
        "abal": 0.3,
        // ohne Ziel-BHD-Angabe in WET2024:
        "qupu": 1.3,
        "soau": 1.0,
        "coav": 1.0
    },
    finalHarvest: true, // um finale Räumung im Anschluss zu ermöglichen
    // constraint: function() {
    // // Lade alle Eichen im Bestand
    // var oakTrees = stand.trees.load('species="quro" or species="qupe"');
    
    // if (oakTrees === 0) {
    //     return false; // Keine Eichen vorhanden
    // }
    
    // // Zähle Eichen mit BHD >= 70 cm
    // var matureOaks = stand.trees.load('(species="quro" or species="qupe") and dbh >= 70');
    
    // // Berechne den Anteil
    // var percentageMature = (matureOaks / oakTrees) * 100;
    
    // // Logging für Debugging
    // fmengine.log("Eichen gesamt: " + oakTrees + ", mit BHD >= 70cm: " + matureOaks + " (" + percentageMature.toFixed(1) + "%)");
    
    // // Rückgabe: true wenn >= 50% der Eichen BHD >= 70 cm haben
    // return percentageMature >= 50;
    // } 
});

// em_Schirm_Tending, em_Schirm_Thinning, em_Schirm_Vorbereitungshieb, em_Schirm_Shelterwood
lib.createSTP('WET_em_Schirm', em_Schirm_Tending, em_Schirm_Thinning, em_Schirm_Vorbereitungshieb, em_Schirm_Shelterwood);

// ------------------------------------------------------------------------------------------------
// WET_e - Eichen-Mischwald geringe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const eg_Femel_Tending = lib.thinning.tending({
    id: 'eg_Femel_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(50/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    }, 
    mode_times: 'dynamic',
    times: Math.round(4*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(5/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false,
});

// Df
// dynamisierte Auslese-Df
const eg_Femel_Thinning = lib.thinning.selectiveThinning({
    id: 'eg_Femel_Thinning',
    schedule: {
        min: Math.round(36/SpeedFactor), 
        opt: Math.round(36/SpeedFactor), 
        max: Math.round(165/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 65, 
    nCompetitors: 12*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,8,8,12,12,13,13,15,15], // Vorratspflege! 2x 15 ergänzt; 2x 8 ergänzt wegen starkem Vorratsanstieg
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.25,
    preferenceFunction: 'height / (stress * 0.8 + 1)', // includes stress factor with a weight of 0.8
    speciesSelectivity: {
        "fasy": 0.4, // Buche reduziert, um Eichen zu fördern
        "quro": 1.5, // Traubeneiche bevorzugt
        "qupe": 1.5, // Stieleiche bevorzugt
        "acps": 1.2, 
        "acpl": 1.2,
        "acca": 1.2,
        "casa": 1.2,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.0,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
     // NB:
        "psme": 0.1,
        "piab": 0.1,
        "pisy": 0.8,
        "pini": 0.8,
        "lade": 0.8,        
        "abal": 0.2,
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Femel
const eg_Femel_SelectPatches = lib.selectOptimalPatches({
    id: 'eg_Femel_SelectPatches',
    schedule: {
        min: Math.round(165/SpeedFactor), 
        opt: Math.round(165/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 6, 
    spacing: 2, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});
const eg_Femel_HarvestPatches = lib.harvest.femel({
    id: 'eg_Femel_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), 
    interval: Math.round(10*SpeedFactor),
    growBy: 2,
    harvestAll: true, // Risiko, dass Saplings aus Femeln gefällt werden
});

const eg_Femel_ClearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

// Pflanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf einer Pflanzung ermittelt
const eg_Femel_Pflanzung = lib.planting.dynamic({
    id: 'eg_Femel_Pflanzung', 
    schedule: { signal: 'Saplings_cleared' },
    //  schedule: {
    //     min: Math.round(82/SpeedFactor), 
    //     opt: Math.round(82/SpeedFactor), 
    //     max: Math.round(82/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 80 = init Femel; Alter 81 = 1. Femel laut log)
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,
    patches: 'patch=1'
});

// eg_Femel_Tending, eg_Femel_Thinning, eg_Femel_SelectPatches, eg_Femel_HarvestPatches, eg_Femel_ClearSaplings, eg_Femel_Pflanzung
lib.createSTP('WET_eg_Femel', eg_Femel_Tending, eg_Femel_Thinning, eg_Femel_SelectPatches, eg_Femel_HarvestPatches, eg_Femel_ClearSaplings, eg_Femel_Pflanzung); // WET_eg_Femel


// ------------------------------------------------------------------------------------------------
// WET_e - Eichen-Mischwald Mittlere Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const em_Femel_Tending = lib.thinning.tending({
    id: 'em_Femel_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(20/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(4*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(5/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false
});

// Df - dynamisierte Auslese-Df  
const em_Femel_Thinning = lib.thinning.selectiveThinning({
    id: 'em_Femel_Thinning',
    schedule: {
        min: Math.round(30/SpeedFactor), 
        opt: Math.round(30/SpeedFactor),
        max: Math.round(150/SpeedFactor),
    },
    mode_times: 'dynamic',
    nTrees: 60, // 40-60 Z-Bäume, mittlerer Wert
    nCompetitors: Math.round(14*SpeedFactor),
    useDynamicIntervals: true,
    intervalPattern: [0,8,8,12,12,13,13,15,15], // Vorratspflege! 2x 15 Jahre ergänzt; entsprechend eg 2x 8 ergänzt
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.25, 
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "fasy": 0.2, // Buche reduziert, um Eichen zu fördern
        "quro": 1.5, // Traubeneiche bevorzugt
        "qupe": 1.5, // Stieleiche bevorzugt
        "acps": 1.2, 
        "acpl": 1.2,
        "acca": 1.2,
        "casa": 1.2,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.0,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
     // NB:
        "psme": 0.1,
        "piab": 0.1,
        "pisy": 0.8,
        "pini": 0.8,
        "lade": 0.8,        
        "abal": 0.2,
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Femel
const em_Femel_SelectPatches = lib.selectOptimalPatches({
    id: 'em_Femel_SelectPatches',
    schedule: {
        min: Math.round(160/SpeedFactor), 
        opt: Math.round(160/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 6, 
    spacing: 2, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});
const em_Femel_HarvestPatches = lib.harvest.femel({
    id: 'em_Femel_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), 
    interval: Math.round(10*SpeedFactor),
    growBy: 2,
    harvestAll: true, // Risiko, dass Saplings aus Femeln gefällt werden
});

const em_Femel_ClearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

// Pflanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf einer Pflanzung ermittelt
const em_Femel_Pflanzung = lib.planting.dynamic({
    id: 'em_Femel_Pflanzung', 
    schedule: { signal: 'Saplings_cleared' },
    //  schedule: {
    //     min: Math.round(82/SpeedFactor), 
    //     opt: Math.round(82/SpeedFactor), 
    //     max: Math.round(82/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 80 = init Femel; Alter 81 = 1. Femel laut log)
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,
    patches: 'patch=1'
});

// em_Femel_Tending, em_Femel_Thinning, em_Femel_SelectPatches, em_Femel_HarvestPatches, em_Femel_ClearSaplings, em_Femel_Pflanzung
lib.createSTP('WET_em_Femel', em_Femel_Tending, em_Femel_Thinning, em_Femel_SelectPatches, em_Femel_HarvestPatches, em_Femel_ClearSaplings, em_Femel_Pflanzung); // WET_em_Femel


// ------------------------------------------------------------------------------------------------
// WET_h - Buntlaubbaum-Mischwald Frisch, einphasige Pflege
// ------------------------------------------------------------------------------------------------

// Jpfl
const h1_Tending = lib.thinning.tending({
    id: 'h1_Tending', 
    schedule: {
        min: Math.round(3/SpeedFactor), 
        opt: Math.round(3/SpeedFactor), 
        max: Math.round(15/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(3*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
});

// dynamisierte Auslese-Df
const h1_Thinning = lib.thinning.selectiveThinning({
    id: 'h1_Thinning',
    schedule: {
        min: Math.round(16/SpeedFactor), 
        opt: Math.round(16/SpeedFactor),
        max: Math.round(65/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 75, // Mittelwert aus 60 bis 90 Z-Bäumen/ha
    nCompetitors: 6*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,8,8,8,8,8,9], // Vorratspflege! kein Eingriff ergänzt
    times: Math.round(8*SpeedFactor),
    decayFactor: 1.3,
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "fasy":0.5,
        "quro":1,
        "qupe":1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.05, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.05, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":0.1,        
        "abal":0.2, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1,
    // zusätzlich aus für SOSFOR: 
        "poni": 1,
        "prse": 0.1, // Risiko Invasivität
        "rops": 0.5, // Risiko Invasivität
    }
});

// Zieldurchmesserernte 
const h1_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'h1_tDBH_harvest',
     schedule: {
        min: Math.round(70/SpeedFactor), 
        opt: Math.round(70/SpeedFactor), 
        max: Math.round(85/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true 
    }, 
    targetDBH: 50/SpeedFactor, 
    times: Math.round(2/SpeedFactor),
    interval: Math.round(7/SpeedFactor),
    dbhList: { 
    //geringes Risiko
        // LB:
        "fasy":60/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":50/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":80/SpeedFactor, //Standard 50
        "piab":50/SpeedFactor, // geästet 60 (aber i.d.R. Standardsortimente)
        "pisy":60/SpeedFactor, // Standard 45
        "pini":60/SpeedFactor, // Standard 45
        "lade":60/SpeedFactor, // Standard 45         
        "abal":80/SpeedFactor, // Standard 50
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
    // zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    },
});

// Räumungshieb am Ende (final harvest activity)
const h1_finalHarvest = lib.harvest.clearcut({
    id: 'h1_finalHarvest',
    schedule: {
        min: Math.round(86/SpeedFactor), 
        opt: Math.round(89/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    // dbhThreshold: 30,
    preferenceFunction: `dbh > 20`,
	finalHarvest: true,
});

// h1_Tending, h1_Thinning, h1_Harvest_tDBH, h1_finalHarvest
lib.createSTP('WET_h1', h1_Tending, h1_Thinning, h1_Harvest_tDBH, h1_finalHarvest); // WET_h1


// ------------------------------------------------------------------------------------------------
// WET_h - Buntlaubbaum-Mischwald Frisch, zweiphasige Pflege
// ------------------------------------------------------------------------------------------------

// Jpfl
const h2_Tending = lib.thinning.tending({
    id: 'h2_Tending', 
    schedule: {
        min: Math.round(3/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(25/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(4/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
});

// dynamisierte Auslese-Df
const h2_Thinning = lib.thinning.selectiveThinning({
    id: 'h2_Thinning',
    schedule: {
        min: Math.round(35/SpeedFactor), 
        opt: Math.round(35/SpeedFactor),
        max: Math.round(70/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 75, // Mittelwert aus 60 bis 90 Z-Bäumen/ha
    nCompetitors: 6*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,7,9,9,10], // Vorratspflege! kein Eingriff nach 10 Jahren ergänzt
    times: Math.round(6*SpeedFactor),
    decayFactor: 1.3,
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "fasy":0.5,
        "quro":1,
        "qupe":1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.05, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.05, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":0.1,        
        "abal":0.2, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1,
    // zusätzlich aus für SOSFOR: 
        "poni": 1,
        "prse": 0.1, // Risiko Invasivität
        "rops": 0.5, // Risiko Invasivität
    }
});

// Zieldurchmesserernte 
const h2_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'h2_tDBH_harvest',
     schedule: {
        min: Math.round(70/SpeedFactor), 
        opt: Math.round(70/SpeedFactor), 
        max: Math.round(75/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true 
    }, 
    targetDBH: 50/SpeedFactor, 
    times: Math.round(1/SpeedFactor),
    interval: Math.round(5/SpeedFactor),
    dbhList: { 
    //geringes Risiko
        // LB:
        "fasy":60/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":50/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":80/SpeedFactor, //Standard 50
        "piab":50/SpeedFactor, // geästet 60 (aber i.d.R. Standardsortimente)
        "pisy":60/SpeedFactor, // Standard 45
        "pini":60/SpeedFactor, // Standard 45
        "lade":60/SpeedFactor, // Standard 45         
        "abal":80/SpeedFactor, // Standard 50
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
    // zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    },
});

// Femel
const h2_SelectPatches = lib.selectOptimalPatches({
    id: 'h2_SelectPatches',
    schedule: {
        min: Math.round(80/SpeedFactor), 
        opt: Math.round(80/SpeedFactor), // 10 Jahre früher als bei WET_bg
        max: Math.round(200/SpeedFactor), // ggf. anpassen: Alter > UTZ, da Ernte in jedem Fall durchgeführt werden muss (auch in älteren Beständen)
        force: true 
    },
    N: Math.round(1*SpeedFactor),
    patchId: 1,
    patchsize: 4,
    spacing: 3,  
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});

const h2_HarvestPatches = lib.harvest.femel({
    id: 'h2_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(1*SpeedFactor),
    growBy: 2, 
    interval: Math.round(10/SpeedFactor),
    harvestAll: true, 
    finalHarvest: true,
});

// lib.createSTP('WET_h2', h2_Tending, h2_Thinning, h2_Harvest_tDBH, h2_SelectPatches, h2_HarvestPatches
lib.createSTP('WET_h2', h2_Tending, h2_Thinning, h2_Harvest_tDBH, h2_SelectPatches, h2_HarvestPatches);


// ------------------------------------------------------------------------------------------------
// WET_j - Buntlaubbaum-Mischwald Trocken
// ------------------------------------------------------------------------------------------------

// Jpfl
const j_Tending = lib.thinning.tending({
    id: 'j_Tending', 
    schedule: {
        min: Math.round(3/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(20/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(4*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(4/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
});

// dynamisierte Auslese-Df
const j_Thinning = lib.thinning.selectiveThinning({
    id: 'j_Thinning',
    schedule: {
        min: Math.round(27/SpeedFactor), 
        opt: Math.round(27/SpeedFactor),
        max: Math.round(70/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 75, // Mittelwert aus 60 bis 90 Z-Bäumen/ha
    nCompetitors: 7*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,8,7,9,10], // Vorratspflege! ein Eingriff nach 10 Jahren ergänzt
    times: Math.round(6*SpeedFactor),
    decayFactor: 1.25,
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "fasy":0.5,
        "quro":1,
        "qupe":1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.1, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.1, // abnehmende Eignung
        "pisy":1.5,
        "pini":1.5,
        "lade":0.1,        
        "abal":0.5, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1,
    // zusätzlich aus für SOSFOR: 
        "poni": 1,
        "prse": 0.1, // Risiko Invasivität
        "rops": 0.5, // Risiko Invasivität
    }
});

// Zieldurchmesserernte 
const j_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'j_tDBH_harvest',
     schedule: {
        min: Math.round(70/SpeedFactor), 
        opt: Math.round(75/SpeedFactor), 
        max: Math.round(80/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true 
    }, 
    targetDBH: 50/SpeedFactor, 
    times: Math.round(1/SpeedFactor),
    interval: Math.round(5/SpeedFactor),
    dbhList: { 
    //geringes Risiko
        // LB:
        "fasy":60/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":50/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":80/SpeedFactor, //Standard 50
        "piab":50/SpeedFactor, // geästet 60 (aber i.d.R. Standardsortimente)
        "pisy":60/SpeedFactor, // Standard 45
        "pini":60/SpeedFactor, // Standard 45
        "lade":60/SpeedFactor, // Standard 45         
        "abal":80/SpeedFactor, // Standard 50
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
    // zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    },
});

// Femel
const j_SelectPatches = lib.selectOptimalPatches({
    id: 'j_SelectPatches',
    schedule: {
        min: Math.round(80/SpeedFactor), 
        opt: Math.round(80/SpeedFactor),
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(1*SpeedFactor),
    patchId: 1,
    patchsize: 6,
    spacing: 5,  
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});

const j_HarvestPatches = lib.harvest.femel({
    id: 'j_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor),
    growBy: 2,
    interval: Math.round(8/SpeedFactor),
    harvestAll: true,
    finalHarvest: true,
});

// j_Tending, j_Thinning, j_Harvest_tDBH, j_SelectPatches, j_HarvestPatches
lib.createSTP('WET_j', j_Tending, j_Thinning, j_Harvest_tDBH, j_SelectPatches, j_HarvestPatches);


// ------------------------------------------------------------------------------------------------
// WET_f - Fichten-Mischwald geringe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const fg_Tending = lib.thinning.tending({
    id: 'fg_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(25/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false 
});

// dynamisierte Auslese-Df
const fg_Thinning = lib.thinning.selectiveThinning({
    id: 'fg_Thinning',
    schedule: {
        min: Math.round(27/SpeedFactor), 
        opt: Math.round(27/SpeedFactor), 
        max: Math.round(70/SpeedFactor),
        force: true
    },
    mode_times: 'dynamic', 
    nTrees: 200, 
    nCompetitors:6*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,4,5,5,5,7,10], // Vorratspflege! ein Eingriff nach 10 Jahren ergänzt
    times: Math.round(8*SpeedFactor),
    decayFactor: 1.35,
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "fasy":0.8,
        "quro":1,
        "qupe":1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.2, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.25, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":0.6, // bei 1.0 wurde relativ häufig Ta ausgezeichnet
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Vorbau in Vorratspflege (~ Alter 55)
    // ToDo: in patches pflanzen; Funktion, die Bedarf an Vorbau Schattbaumarten ermittelt
const fg_SelectPatches_Vorbau = lib.selectOptimalPatches({
    id: 'fg_SelectPatches_Vorbau',
     schedule: {
        min: Math.round(60/SpeedFactor), 
        opt: Math.round(65/SpeedFactor), 
        max: Math.round(70/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 2,
    patchsize: 2,
    spacing: 5,
    criterium: 'max_basalarea_absolute', // nicht vitale sollen als erstes entnommen werden (Was ist mit räumlicher Ordnung?)
    sendSignal: 'Patches_Vorbau'
});

const fg_clearSaplings_Vorbau = { type: 'general',
    schedule: { signal: 'Patches_Vorbau' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared_Vorbau');
}};

const fg_Vorbau = lib.planting.dynamic({
    id: 'fg_Vorbau',
    schedule: { signal: 'Saplings_cleared_Vorbau' },
    // schedule: {
    //     min: Math.round(50/SpeedFactor), 
    //     opt: Math.round(55/SpeedFactor), 
    //     max: Math.round(60/SpeedFactor), 
    //     force: true 
    // },
    speciesSelectivity: {
        'abal': 0.5,
        'fasy': 0.5},
    speciesDefaults: lib.planting.speciesDefaults,
    patches: 'patch=2'
});

const fg_SelectPatches_Femel = lib.selectOptimalPatches({
    id: 'fg_SelectPatches_Femel',
     schedule: {
        min: Math.round(70/SpeedFactor), 
        opt: Math.round(70/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 4,
    spacing: 2,
    criterium: 'max_basalarea_absolute', // nicht vitale sollen als erstes entnommen werden (Was ist mit räumlicher Ordnung?)
    sendSignal: 'PatchesSelected'
});

const fg_HarvestPatches = lib.harvest.femel({
    id: 'fg_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), // number of consecutive enlargement steps after start (macht trotzdem 3 Entnahmen)
    interval: Math.round(10*SpeedFactor), // years between each step
    growBy: 2, // number of "rings" of 10 m cells to grow each step
    harvestAll: true, // überprüfen, ob bereits etablierte Verjüngung entfernt wird! - ggf. false setzen & finalHarvest activity erstellen
    //sendSignal: 'Pflanzung'
});

// fg_Tending, fg_Thinning, fg_SelectPatches_Vorbau, fg_clearSaplings_Vorbau, fg_Vorbau, fg_Harvest_tDBH, fg_SelectPatches_Femel, fg_HarvestPatches
lib.createSTP('WET_fg', fg_Tending, fg_Thinning, fg_SelectPatches_Vorbau, fg_clearSaplings_Vorbau, fg_Vorbau, fg_SelectPatches_Femel, fg_HarvestPatches); // WET_fg


// ------------------------------------------------------------------------------------------------
// WET_f - Fichten-Mischwald Mittlere Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const fm_Tending = lib.thinning.tending({
    id: 'fm_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(30/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false 
});

// dynamisierte Auslese-Df
const fm_Thinning = lib.thinning.selectiveThinning({
    id: 'fm_Thinning',
    schedule: {
        min: Math.round(27/SpeedFactor), 
        opt: Math.round(27/SpeedFactor),
        max: Math.round(55/SpeedFactor),
        force: true
    },
    mode_times: 'dynamic',
    nTrees: 200, 
    nCompetitors:8*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,4,5,5,5,7],
    times: Math.round(7*SpeedFactor),
    decayFactor: 1.3,
    preferenceFunction: 'height / (stress * 0.8 + 1)',  
    speciesSelectivity: {
        "fasy":0.8,
        "quro":1.5,
        "qupe":1.5,
        "acps":1.5, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.15, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.15, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":0.7, // bei 1.0 wurde relativ häufig Ta ausgezeichnet
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Zieldurchmesserernte (beginnend ab ADf)
const fm_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'fm_tDBH_harvest',
     schedule: {
        min: Math.round(60/SpeedFactor), 
        opt: Math.round(60/SpeedFactor), 
        max: Math.round(200/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true 
    }, 
    targetDBH: 50/SpeedFactor, 
    times: Math.round(1/SpeedFactor), // macht trotzdem nur 1 Durchlauf
    interval: Math.round(5/SpeedFactor),
    dbhList: { 
        // mittleres + hohes Risiko
        // LB:
        "fasy":50/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":45/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":50/SpeedFactor, 
        "piab":50/SpeedFactor, 
        "pisy":45/SpeedFactor, 
        "pini":45/SpeedFactor,
        "lade":45/SpeedFactor,         
        "abal":50/SpeedFactor, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
// zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    }, 
});  

// Femel
const fm_SelectPatches_Femel = lib.selectOptimalPatches({
    id: 'fm_SelectPatches_Femel',
     schedule: {
        min: Math.round(70/SpeedFactor), 
        opt: Math.round(70/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 4,
    spacing: 2,
    criterium: 'max_basalarea_absolute', // nicht vitale sollen als erstes entnommen werden (Was ist mit räumlicher Ordnung?)
    sendSignal: 'PatchesSelected'
});

const fm_HarvestPatches = lib.harvest.femel({
    id: 'fm_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), // number of consecutive enlargement steps after start (macht trotzdem 3 Entnahmen)
    interval: Math.round(10*SpeedFactor), // years between each step
    growBy: 2, // number of "rings" of 10 m cells to grow each step
    harvestAll: true, // überprüfen, ob bereits etablierte Verjüngung entfernt wird! - ggf. false setzen & finalHarvest activity erstellen
    //sendSignal: 'Pflanzung'
});

const fm_clearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

// Pflanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf einer Pflanzung ermittelt
const fm_Pflanzung = lib.planting.dynamic({
    id: 'fm_Pflanzung', 
    schedule: { signal: 'Saplings_cleared' },
    //  schedule: {
    //     min: Math.round(82/SpeedFactor), 
    //     opt: Math.round(82/SpeedFactor), 
    //     max: Math.round(82/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 80 = init Femel; Alter 81 = 1. Femel laut log)
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,
    patches: 'patch=1'
});

// fm_Tending, fm_Thinning, fm_Harvest_tDBH, fm_SelectPatches_Femel, fm_HarvestPatches, fm_clearSaplings, fm_Pflanzung
lib.createSTP('WET_fm', fm_Tending, fm_Thinning, fm_Harvest_tDBH, fm_SelectPatches_Femel, fm_HarvestPatches, fm_clearSaplings, fm_Pflanzung); // WET_fm


// ------------------------------------------------------------------------------------------------
// WET_fx - Fichtenmischwald Hohe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const fx_Tending = lib.thinning.tending({
    id: 'fx_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(30/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(4/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
});

// dynamisierte Auslese-Df
const fx_Thinning = lib.thinning.selectiveThinning({
    id: 'fx_Thinning',
    schedule: {
        min: Math.round(27/SpeedFactor), 
        opt: Math.round(27/SpeedFactor),
        max: Math.round(50/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 200, // Mittel aus 100 bis 200 Z-Bäumen
    nCompetitors:8*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,4,5,5,5],
    times: Math.round(6*SpeedFactor),
    decayFactor: 1.3,
    preferenceFunction: 'height / (stress * 1.5 + 1)', // includes stress factor with a weight of 1.5
    speciesSelectivity: {
        "fasy":0.75,
        "quro":1.5,
        "qupe":1.5,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.2, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.01, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":1.5, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Femel
const fx_SelectPatches = lib.selectOptimalPatches({
    id: 'fx_SelectPatches',
    schedule: {
        min: Math.round(60/SpeedFactor), 
        opt: Math.round(60/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(1*SpeedFactor),
    patchId: 1,
    patchsize: 6, 
    spacing: 3, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});
const fx_HarvestPatches = lib.harvest.femel({
    id: 'fx_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), // number of consecutive enlargement steps after start
    interval: Math.round(10*SpeedFactor), // years between each step
    growBy: 2, // number of "rings" of 10 m cells to grow each step
    harvestAll: true, // überprüfen, ob bereits etablierte Verjüngung entfernt wird! - ggf. false setzen & finalHarvest activity erstellen
    // constraint: function() { return stand.absoluteAge() <= 75; },
    // finalHarvest: true,
    // preferenceFunction: 'dbh > 15',
});

const fx_clearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

// Pflanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf einer Pflanzung ermittelt
const fx_Pflanzung = lib.planting.dynamic({
    id: 'fx_Pflanzung', 
    schedule: { signal: 'Saplings_cleared' },
    //   schedule: {
    //      min: Math.round(63/SpeedFactor), 
    //      opt: Math.round(63/SpeedFactor), 
    //      max: Math.round(63/SpeedFactor), 
    //      force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 80 = init Femel; Alter 81 = 1. Femel laut log)
    patches: 'patch=1',
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,    
});

// fx_Tending, fx_Thinning, fx_SelectPatches, fx_HarvestPatches, fx_clearSaplings, fx_Pflanzung
lib.createSTP('WET_fx', fx_Tending, fx_Thinning, fx_SelectPatches, fx_HarvestPatches, fx_clearSaplings, fx_Pflanzung);


// ------------------------------------------------------------------------------------------------
// WET_t - Tannen-Mischwald Geringe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const tg_Tending = lib.thinning.tending({
    id: 'tg_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(30/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
    // constraint: add top height constraint?
});

// dynamisierte Auslese-Df
const tg_Thinning = lib.thinning.selectiveThinning({
    id: 'tg_Thinning',
    schedule: {
        min: Math.round(27/SpeedFactor), 
        opt: Math.round(27/SpeedFactor),
        max: Math.round(65/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 100, // Mittelwert aus 100 bis 200 Z-Bäumen/ha
    nCompetitors:15*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,5,5,5,5,6],
    times: Math.round(7*SpeedFactor),
    decayFactor: 1.3,
    preferenceFunction: 'height / (stress * 0.8 + 1)',  
    speciesSelectivity: {
        "fasy":1,
        "quro":1,
        "qupe":1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.1, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.1, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":1.3, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Zieldurchmesserernte (beginnend ab ADf)
const tg_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'tg_tDBH_harvest',
     schedule: {
        min: Math.round(65/SpeedFactor), 
        opt: Math.round(65/SpeedFactor), 
        max: Math.round(85/SpeedFactor), // max. Alter > UTZ, da auch ältere Bestände harvest activities unterzogen werden sollen
        force: true 
    }, 
    targetDBH: 50/SpeedFactor, 
    times: Math.round(5/SpeedFactor),
    interval: Math.round(6/SpeedFactor),
    dbhList: { 
    //geringes Risiko
        // LB:
        "fasy":60/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":50/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":80/SpeedFactor, //Standard 50
        "piab":50/SpeedFactor, // geästet 60 (aber i.d.R. Standardsortimente)
        "pisy":60/SpeedFactor, // Standard 45
        "pini":60/SpeedFactor, // Standard 45
        "lade":60/SpeedFactor, // Standard 45         
        "abal":80/SpeedFactor, // Standard 50
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
    // zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    }, 
});

// Räumungshieb am Ende (final harvest activity)
const tg_finalHarvest = lib.harvest.clearcut({
    id: 'tg_finalHarvest',
    schedule: {
        min: Math.round(85/SpeedFactor), 
        opt: Math.round(90/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    // dbhThreshold: 30,
    preferenceFunction: `dbh > 20`,
	finalHarvest: true,
});

// tg_Tending, tg_Thinning, tg_Harvest_tDBH, tg_finalHarvest
lib.createSTP('WET_tg', tg_Tending, tg_Thinning, tg_Harvest_tDBH, tg_finalHarvest); // WET_tg


// ------------------------------------------------------------------------------------------------
// WET_t - Tannen-Mischwald Mittlere Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const tm_Tending = lib.thinning.tending({
    id: 'tm_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(30/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
    // constraint: add top height constraint?
});

// dynamisierte Auslese-Df
const tm_Thinning = lib.thinning.selectiveThinning({
    id: 'tm_Thinning',
    schedule: {
        min: Math.round(27/SpeedFactor), 
        opt: Math.round(27/SpeedFactor),
        max: Math.round(65/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 150, // Mittelwert aus 100 bis 200 Z-Bäumen/ha
    nCompetitors:15*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,5,5,5,5,6],
    times: Math.round(7*SpeedFactor),
    decayFactor: 1.3,
    preferenceFunction: 'height / (stress * 0.8 + 1)',  
    speciesSelectivity: {
        "fasy":1,
        "quro":1,
        "qupe":1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.1, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.1, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":1.2, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// // Vorbau nach JDf (~ Alter 55)
//     // ToDo: in patches pflanzen; Funktion, die Bedarf an Vorbau Schattbaumarten ermittelt
// const tm_SelectPatches_Vorbau = lib.selectOptimalPatches({
//     id: 'tm_SelectPatches_Vorbau',
//      schedule: {
//         min: Math.round(45/SpeedFactor), 
//         opt: Math.round(50/SpeedFactor), 
//         max: Math.round(55/SpeedFactor), 
//         force: true 
//     },
//     N: Math.round(2*SpeedFactor),
//     patchId: 2,
//     patchsize: 2,
//     spacing: 5,
//     criterium: 'max_basalarea_absolute', // nicht vitale sollen als erstes entnommen werden (Was ist mit räumlicher Ordnung?)
//     sendSignal: 'Patches_Vorbau'
// });

// const tm_clearSaplings_Vorbau = { type: 'general',
//     schedule: { signal: 'Patches_Vorbau' },
//     action: function(){
//         stand.simulate=false;
//         // stand.trees.simulate=false;
//         stand.trees.loadAll();
//         var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
//         fmengine.log("Killed " + removedSaplings + " saplings");
//         // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
//         // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
//         stand.stp.signal('Saplings_cleared_Vorbau');
// }};

// const tm_Vorbau = lib.planting.dynamic({
//     id: 'tm_Vorbau',
//     schedule: { signal: 'Saplings_cleared_Vorbau' },
//     // schedule: {
//     //     min: Math.round(50/SpeedFactor), 
//     //     opt: Math.round(51/SpeedFactor), 
//     //     max: Math.round(60/SpeedFactor), 
//     //     force: true 
//     // },
//     speciesSelectivity: {
//         'abal': 0.5,
//         'fasy': 0.5},
//     speciesDefaults: lib.planting.speciesDefaults,
//     patches: 'patch=2'
// });

// Zieldurchmesserernte (beginnend ab ADf)
const tm_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'tm_tDBH_harvest',
     schedule: {
        min: Math.round(55/SpeedFactor), 
        opt: Math.round(60/SpeedFactor), 
        max: Math.round(65/SpeedFactor), 
        force: true 
    }, 
    targetDBH: 50/SpeedFactor, 
    times: Math.round(2/SpeedFactor),
    interval: Math.round(5/SpeedFactor),
    dbhList: { 
    // LB:
        "fasy":60/SpeedFactor,// schlechtere 50
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":50/SpeedFactor, // mind. 40
        "acca":50/SpeedFactor, // mind. 40
        "casa":50/SpeedFactor, // 40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, // Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, // Priorität: Erhaltung
        "cabe":50/SpeedFactor, // mind. 40
        "tico":50/SpeedFactor, // mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":50/SpeedFactor, // mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, // Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB: (Standardsortimente)
        "psme":50/SpeedFactor, // beste 80
        "piab":50/SpeedFactor, // beste 50
        "pisy":45/SpeedFactor, // beste 60
        "pini":45/SpeedFactor, // beste 60
        "lade":45/SpeedFactor, // beste 60         
        "abal":50/SpeedFactor, // beste 80
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
    // // zusätzlich aus für SOSFOR: 
    //     "poni": 60/SpeedFactor, // WET24: 60
    //     "prse": 50/SpeedFactor, // keine Angabe in WET24
    //     "rops": 50/SpeedFactor, // keine Angabe in WET24
    //     "rest": 40/SpeedFactor, // Wo wird das definiert?    
    }, 
});

// Femel
const tm_SelectPatches = lib.selectOptimalPatches({
    id: 'tm_SelectPatches',
    schedule: {
        min: Math.round(65/SpeedFactor), 
        opt: Math.round(70/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 5, 
    spacing: 2, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});

const tm_HarvestPatches = lib.harvest.femel({
    id: 'tm_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), // number of consecutive enlargement steps after start
    interval: Math.round(10*SpeedFactor), // years between each step
    growBy: 1, // number of "rings" of 10 m cells to grow each step
    harvestAll: true, // Risiko, dass Saplings aus Femeln gefällt werden
    // constraint: function() { return stand.absoluteAge() <= 75; },
    //sendSignal: 'Pflanzung_Lichtbaumarten'
});

const tm_clearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

// Pfanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf an Lichtbaumarten ermittelt
const tm_Pflanzung = lib.planting.dynamic({
    id: 'tm_Pflanzung',
    schedule: { signal: 'Saplings_cleared' },
    // schedule: {
    //     min: Math.round(97/SpeedFactor), 
    //     opt: Math.round(97/SpeedFactor), 
    //     max: Math.round(97/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 95 = init Femel; Alter 96 = 1. Femel laut log)
    // //schedule: { signal: 'Pflanzung_Lichtbaumarten' },
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,
    patches: 'patch=1'
});

// tm_Tending, tm_Thinning, tm_SelectPatches_Vorbau, tm_clearSaplings_Vorbau, tm_Vorbau, tm_SelectPatches, tm_HarvestPatches, tm_clearSaplings, tm_Pflanzung
lib.createSTP('WET_tm', tm_Tending, tm_Thinning, tm_SelectPatches, tm_HarvestPatches, tm_clearSaplings, tm_Pflanzung); // WET_tm


// ------------------------------------------------------------------------------------------------
// WET_tx - Tannen-Mischwald Hohe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const tx_Tending = lib.thinning.tending({
    id: 'tx_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(30/SpeedFactor), // 5 Jahre vor 1. Df
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor), // im log checken, wie oft es tatsächlich ausgeführt wird!
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
    // constraint: add top height constraint?
});

// dynamisierte Auslese-Df
const tx_Thinning = lib.thinning.selectiveThinning({
    id: 'tx_Thinning',
    schedule: {
        min: Math.round(26/SpeedFactor), 
        opt: Math.round(26/SpeedFactor),
        max: Math.round(65/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 150, // Mittelwert aus 100 bis 200 Z-Bäumen/ha
    nCompetitors:15*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,5,5,5,5,6],
    times: Math.round(7*SpeedFactor),
    decayFactor: 1.3,
    preferenceFunction: 'height / (stress * 1.5 + 1)', // includes stress factor with a weight of 1.5
    speciesSelectivity: {
        "fasy":1,
        "quro":1.5,
        "qupe":1.5,
        "acps":1.5, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.5, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.2, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":0.5, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Zieldurchmesserernte (beginnend ab ADf)
const tx_Harvest_tDBH = lib.harvest.targetDBH({
    id: 'tx_Harvest_tDBH',
     schedule: {
        min: Math.round(55/SpeedFactor), 
        opt: Math.round(60/SpeedFactor), 
        max: Math.round(75/SpeedFactor), 
        force: true 
    }, 
    targetDBH: 50/SpeedFactor, 
    times: Math.round(1/SpeedFactor),
    interval: Math.round(5/SpeedFactor),
    dbhList: { 
        // mittleres + hohes Risiko
        // LB:
        "fasy":50/SpeedFactor,//schlechtere 50, 65 bei 70 Z-Bäumen
        "quro":70/SpeedFactor,
        "qupe":70/SpeedFactor,
        "acps":50/SpeedFactor, 
        "acpl":45/SpeedFactor, //mind. 40
        "acca":45/SpeedFactor, //mind. 40
        "casa":50/SpeedFactor, //40 bzw. 50 - 60
        "bepe":45/SpeedFactor,
        "soar":40/SpeedFactor, //Ersatz für Speierling & Elsbeere
        "frex":500/SpeedFactor, //Priorität: Erhaltung
        "cabe":45/SpeedFactor, //mind. 40
        "tico":45/SpeedFactor, //mind. 40 (Sommerlinde nicht in iLand aufgenommen)
        "potr":60/SpeedFactor,
        "quru":45/SpeedFactor, //mind. 40
        "algl":50/SpeedFactor,
        "ulgl":500/SpeedFactor, //Priorität: Erhaltung
        "saca":45/SpeedFactor,
     // NB:
        "psme":50/SpeedFactor, 
        "piab":50/SpeedFactor, 
        "pisy":45/SpeedFactor, 
        "pini":45/SpeedFactor,
        "lade":45/SpeedFactor,         
        "abal":50/SpeedFactor, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":40/SpeedFactor,
        "soau":40/SpeedFactor,
        "coav":40/SpeedFactor,
// zusätzlich aus für SOSFOR: 
        "poni": 60/SpeedFactor, // WET24: 60
        "prse": 40/SpeedFactor, // keine Angabe in WET24 (vgl. Wildobst)
        "rops": 45/SpeedFactor, // keine Angabe in WET24
    }, 
});

// Femel
const tx_SelectPatches = lib.selectOptimalPatches({
    id: 'tx_SelectPatches',
    schedule: {
        min: Math.round(70/SpeedFactor), 
        opt: Math.round(70/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 6, 
    spacing: 4, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});

const tx_HarvestPatches = lib.harvest.femel({
    id: 'tx_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), // number of consecutive enlargement steps after start
    interval: Math.round(10*SpeedFactor), // years between each step
    growBy: 1, // number of "rings" of 10 m cells to grow each step
    harvestAll: true, // Risiko, dass Saplings aus Femeln gefällt werden
    // constraint: function() { return stand.absoluteAge() <= 75; },
    //sendSignal: 'Pflanzung_Lichtbaumarten'
});

const tx_clearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

// Pfanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf an Lichtbaumarten ermittelt
const tx_Pflanzung = lib.planting.dynamic({
    id: 'tx_Pflanzung',
    schedule: { signal: 'Saplings_cleared' },
    // schedule: {
    //     min: Math.round(97/SpeedFactor), 
    //     opt: Math.round(97/SpeedFactor), 
    //     max: Math.round(97/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 95 = init Femel; Alter 96 = 1. Femel laut log)
    // //schedule: { signal: 'Pflanzung_Lichtbaumarten' },
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,
    patches: 1 
});

// tx_Tending, tx_Thinning, tx_SelectPatches, tx_HarvestPatches, tx_clearSaplings, tx_Pflanzung
lib.createSTP('WET_tx', tx_Tending, tx_Thinning, tx_SelectPatches, tx_HarvestPatches, tx_clearSaplings, tx_Pflanzung); // WET_tx


// ------------------------------------------------------------------------------------------------
// WET_d - Douglasien-Mischwald Geringe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const dg_Tending = lib.thinning.tending({
    id: 'dg_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(20/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(4*SpeedFactor), 
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
});

// dynamisierte Auslese-Df
const dg_Thinning = lib.thinning.selectiveThinning({
    id: 'dg_Thinning',
    schedule: {
        min: Math.round(28/SpeedFactor), 
        opt: Math.round(28/SpeedFactor),
        max: Math.round(65/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 120, 
    nCompetitors:9*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,3,4,4,4,4,4,5,10], // Vorratspflege! einmal 10 Jahre ergänzt
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.35,
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "fasy":1,
        "quro":1,
        "qupe":1,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.3, // bei 1.0 wurde relativ häufig Dgl ausgezeichnet
        "piab":0.2, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":1, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Femel
const dg_SelectPatches = lib.selectOptimalPatches({
    id: 'dg_SelectPatches',
    schedule: {
        min: Math.round(65/SpeedFactor), 
        opt: Math.round(70/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 3, 
    spacing: 4, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});
const dg_HarvestPatches = lib.harvest.femel({
    id: 'dg_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), // number of consecutive enlargement steps after start
    interval: Math.round(10*SpeedFactor), // größer als bei WET_tm
    growBy: 1, // number of "rings" of 10 m cells to grow each step
    harvestAll: true, // Risiko, dass Saplings aus Femeln gefällt werden
    // constraint: function() { return stand.absoluteAge() <= 75; },
});

lib.createSTP('WET_dg', dg_Tending, dg_Thinning, dg_SelectPatches, dg_HarvestPatches); // WET_dg


// ------------------------------------------------------------------------------------------------
// WET_d - Douglasien-Mischwald Mitllere Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const dm_Tending = lib.thinning.tending({
    id: 'dm_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(20/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    times: Math.round(4*SpeedFactor), 
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10,
    block: false
});

// dynamisierte Auslese-Df
const dm_Thinning = lib.thinning.selectiveThinning({
    id: 'dm_Thinning',
    schedule: {
        min: Math.round(27/SpeedFactor), 
        opt: Math.round(27/SpeedFactor),
        max: Math.round(65/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 120, 
    nCompetitors:14*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,3,4,4,4,4,4,5,10], // Vorratspflege! einmal 10 Jahre ergänzt
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.3,
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
        "fasy":1,
        "quro":1.4,
        "qupe":1.4,
        "acps":1, 
        "acpl":1,
        "acca":1,
        "casa":1,
        "bepe":1,
        "soar":1,
        "frex":1,
        "cabe":1,
        "tico":1,
        "potr":1,
        "quru":1,
        "algl":1,
        "ulgl":1,
        "saca":1,
     // NB:
        "psme":0.13, // geringer als bei WET_dg
        "piab":0.1, // abnehmende Eignung
        "pisy":1,
        "pini":1,
        "lade":1,        
        "abal":1, 
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu":1,
        "soau":1,
        "coav":1, 
    }
});

// Femel
const dm_SelectPatches = lib.selectOptimalPatches({
    id: 'dm_SelectPatches',
    schedule: {
        min: Math.round(70/SpeedFactor), 
        opt: Math.round(70/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 6, 
    spacing: 2, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});
const dm_HarvestPatches = lib.harvest.femel({
    id: 'dm_HarvestPatches',
    schedule: { signal: 'PatchesSelected' }, 
    steps: Math.round(2*SpeedFactor), 
    interval: Math.round(10*SpeedFactor),
    growBy: 2, // zügiger Rändeln als bei WET_dg
    harvestAll: true, // Risiko, dass Saplings aus Femeln gefällt werden
    // constraint: function() { return stand.absoluteAge() <= 75; },
});

const dm_clearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

// Pflanzung
    // ToDo: in patches pflanzen; Funktion, die Bedarf einer Pflanzung ermittelt
const dm_Pflanzung = lib.planting.dynamic({
    id: 'dm_Pflanzung', 
    schedule: { signal: 'Saplings_cleared' },
    //  schedule: {
    //     min: Math.round(82/SpeedFactor), 
    //     opt: Math.round(82/SpeedFactor), 
    //     max: Math.round(82/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 80 = init Femel; Alter 81 = 1. Femel laut log)
    patches: 'patch=1',
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults,
});

// dm_Tending, dm_Thinning, dm_SelectPatches, dm_HarvestPatches, dm_clearSaplings, dm_Pflanzung
lib.createSTP('WET_dm', dm_Tending, dm_Thinning, dm_SelectPatches, dm_HarvestPatches, dm_clearSaplings, dm_Pflanzung); // WET_dm


// ------------------------------------------------------------------------------------------------
// WET_k - Kiefern-Mischwald Geringe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const kg_Tending = lib.thinning.tending({
    id: 'kg_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(20/SpeedFactor),
        force: true 
    }, 
    mode_times: 'dynamic',
    times: Math.round(5*SpeedFactor),
    interval: Math.round(4/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false,
    // constraint: add top height constraint?
});

// Df
// dynamisierte Auslese-Df
const kg_Thinning = lib.thinning.selectiveThinning({
    id: 'kg_Thinning',
    schedule: {
        min: Math.round(28/SpeedFactor), 
        opt: Math.round(28/SpeedFactor), 
        max: Math.round(125/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 65, 
    nCompetitors:14*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,8,8,9,9,9,9,9,10], // Vorratspflege! 1x 10 Jahre ergänzt; 2x 8 Jahre ergänzt wegen starkem Vorratsanstieg
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.4,
    preferenceFunction: 'height / (stress * 0.8 + 1)',
    speciesSelectivity: {
        "fasy": 0.8,
        "quro": 1.0,
        "qupe": 1.0, 
        "acps": 1.0, 
        "acpl": 1.0,
        "acca": 1.0,
        "casa": 1.0,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.0,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
     // NB:
        "psme": 0.1,
        "piab": 0.1,
        "pisy": 1.5,
        "pini": 1.5,
        "lade": 1.0,        
        "abal": 0.2,
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu": 1.0,
        "soau": 1.0,
        "coav": 1.0,
    }
});    

// Schirmschlag für geringe Risikoeinstufung //
// Vorbereitungshieb (Auszug Unter- und Zwischenstand)
const kg_Vorbereitungshieb = { type: 'general',
    schedule: {
        min: Math.round(125/SpeedFactor), 
        opt: Math.round(129/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and height<15');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
}};

// Schirmschlag für geringe Risikoeinstufung
const kg_Shelterwood = lib.harvest.shelterwood({
    id: 'kg_Shelterwood',
    // schedule: { signal : 'Auszug_completed' },
    schedule: {
        min: Math.round(126/SpeedFactor), 
        opt: Math.round(130/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    nTrees: Math.round(65*SpeedFactor), // = Z-Baumzahl in Df
    nCompetitors: Math.round(1000*SpeedFactor), // So hoch, um sicherzustellen, dass nur die Samenbäume verbleiben?
    interval: Math.round(3/SpeedFactor), 
    times: 3, // = 1 Besamungshieb, 2 Lichtungshiebe
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
       // LB:
        "quro": 2.5, // sehr starke Bevorzugung
        "qupe": 2.5,
        "fasy": 0.3, // Buche stark reduziert, um Eichen zu fördern
        "acps": 1.0, 
        "acpl": 1.0,
        "acca": 1.0,
        "casa": 1.0,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.3,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
        // NB:
        "psme": 0.3,
        "piab": 0.1,
        "pisy": 0.9,
        "pini": 0.9,
        "lade": 0.6,
        "abal": 0.3,
        // ohne Ziel-BHD-Angabe in WET2024:
        "qupu": 1.3,
        "soau": 1.0,
        "coav": 1.0
    },
    finalHarvest: true, // um finale Räumung im Anschluss zu ermöglichen
    // constraint: function() {
    // // Lade alle Eichen im Bestand
    // var oakTrees = stand.trees.load('species="quro" or species="qupe"');
    
    // if (oakTrees === 0) {
    //     return false; // Keine Eichen vorhanden
    // }
    
    // // Zähle Eichen mit BHD >= 70 cm
    // var matureOaks = stand.trees.load('(species="quro" or species="qupe") and dbh >= 70');
    
    // // Berechne den Anteil
    // var percentageMature = (matureOaks / oakTrees) * 100;
    
    // // Logging für Debugging
    // fmengine.log("Eichen gesamt: " + oakTrees + ", mit BHD >= 70cm: " + matureOaks + " (" + percentageMature.toFixed(1) + "%)");
    
    // // Rückgabe: true wenn >= 50% der Eichen BHD >= 70 cm haben
    // return percentageMature >= 50;
    // },
    // sendSignal: 'Shelterwood_completed'
});

// kg_Tending, kg_Thinning, kg_Vorbereitungshieb, kg_Shelterwood
lib.createSTP('WET_kg', kg_Tending, kg_Thinning, kg_Vorbereitungshieb, kg_Shelterwood);


// ------------------------------------------------------------------------------------------------
// WET_k - Kiefern-Mischwald Mittlere Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const km_Tending = lib.thinning.tending({
    id: 'km_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(20/SpeedFactor),
        force: true 
    }, 
    mode_times: 'dynamic',
    times: Math.round(8*SpeedFactor),
    interval: Math.round(5/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false,
    // constraint: add top height constraint?
});

// Df
// dynamisierte Auslese-Df
const km_Thinning = lib.thinning.selectiveThinning({
    id: 'km_Thinning',
    schedule: {
        min: Math.round(59/SpeedFactor), 
        opt: Math.round(59/SpeedFactor), 
        max: Math.round(125/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic',
    nTrees: 65, 
    nCompetitors: 14*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,8,8,9,9,9,9,9,10], // Vorratspflege! 1x 10 Jahre ergänzt; 2x 8 Jahre ergänzt wegen starkem Vorratsanstieg
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.4,
    preferenceFunction: 'height / (stress * 0.8 + 1)',
    speciesSelectivity: {
        "fasy": 0.8,
        "quro": 1.0,
        "qupe": 1.0, 
        "acps": 1.0, 
        "acpl": 1.0,
        "acca": 1.0,
        "casa": 1.0,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.0,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
     // NB:
        "psme": 0.1,
        "piab": 0.01,
        "pisy": 0.5,
        "pini": 0.75,
        "lade": 0.3,        
        "abal": 0.2,
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu": 1.0,
        "soau": 1.0,
        "coav": 1.0,
    }
});    

// Schirmschlag für mittlere Risikoeinstufung //
// Vorbereitungshieb (Auszug Unter- und Zwischenstand)
const km_Vorbereitungshieb = { type: 'general',
    schedule: {
        min: Math.round(125/SpeedFactor), 
        opt: Math.round(129/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and height<15');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
}};

// Schirmschlag für mittlere Risikoeinstufung
const km_Shelterwood = lib.harvest.shelterwood({
    id: 'km_Shelterwood',
    // schedule: { signal : 'Auszug_completed' },
    schedule: {
        min: Math.round(126/SpeedFactor), 
        opt: Math.round(130/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    nTrees: Math.round(65*SpeedFactor), // = Z-Baumzahl in Df
    nCompetitors: Math.round(1000*SpeedFactor), // So hoch, um sicherzustellen, dass nur die Samenbäume verbleiben?
    interval: Math.round(3/SpeedFactor), 
    times: 3, // = 1 Besamungshieb, 2 Lichtungshiebe
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
       // LB:
        "quro": 2.5, // sehr starke Bevorzugung
        "qupe": 2.5,
        "fasy": 0.3, // Buche stark reduziert, um Eichen zu fördern
        "acps": 1.0, 
        "acpl": 1.0,
        "acca": 1.0,
        "casa": 1.0,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.3,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
        // NB:
        "psme": 0.3,
        "piab": 0.1,
        "pisy": 0.9,
        "pini": 0.9,
        "lade": 0.6,
        "abal": 0.3,
        // ohne Ziel-BHD-Angabe in WET2024:
        "qupu": 1.3,
        "soau": 1.0,
        "coav": 1.0
    },
    finalHarvest: true,
});

// km_Tending, km_Thinning, km_Vorbereitungshieb, km_Shelterwood
lib.createSTP('WET_km', km_Tending, km_Thinning, km_Vorbereitungshieb, km_Shelterwood);


// ------------------------------------------------------------------------------------------------
// WET_kx - Kiefern-Mischwald Hohe Risikoeinstufung
// ------------------------------------------------------------------------------------------------

// Jpfl
const kx_Tending = lib.thinning.tending({
    id: 'kx_Tending', 
    schedule: {
        min: Math.round(5/SpeedFactor), 
        opt: Math.round(5/SpeedFactor), 
        max: Math.round(30/SpeedFactor),
        force: true 
    }, 
    mode_times: 'dynamic',
    times: Math.round(4*SpeedFactor),
    interval: Math.round(3/SpeedFactor), 
    speciesSelectivity: function() {return dynamicTendingSelectivity()},
    intensity: 10, 
    block: false,
});

// Df
// dynamisierte Auslese-Df
const kx_Thinning = lib.thinning.selectiveThinning({
    id: 'kx_Thinning',
    schedule: {
        min: Math.round(21/SpeedFactor), 
        opt: Math.round(21/SpeedFactor), 
        max: Math.round(125/SpeedFactor),
        force: true 
    },
    mode_times: 'dynamic', 
    nTrees: 65, 
    nCompetitors: 14*SpeedFactor,
    useDynamicIntervals: true,
    intervalPattern: [0,8,8,9,9,9,9,9,10],
    times: Math.round(10*SpeedFactor),
    decayFactor: 1.4,
    preferenceFunction: 'height / (stress * 0.8 + 1)',
    speciesSelectivity: {
        "fasy": 0.8,
        "quro": 1.5,
        "qupe": 1.5, 
        "acps": 1.3, 
        "acpl": 1.3,
        "acca": 1.3,
        "casa": 1.0,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.3,
        "potr": 1.0,
        "quru": 1.0,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
     // NB:
        "psme": 0.1,
        "piab": 0.2,
        "pisy": 0.5,
        "pini": 1.0,
        "lade": 0.2,        
        "abal": 0.2,
     // ohne Ziel-BHD-Angabe in WET2024:
        "qupu": 1.3,
        "soau": 1.0,
        "coav": 1.0,
    }
});    

// Schirmschlag für hohe Risikoeinstufung //
// Vorbereitungshieb (Auszug Unter- und Zwischenstand)
const kx_Vorbereitungshieb = { type: 'general',
    schedule: {
        min: Math.round(105/SpeedFactor), 
        opt: Math.round(109/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and height<15');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
}};

// Schirmschlag für hohe Risikoeinstufung
const kx_Shelterwood = lib.harvest.shelterwood({
    id: 'kx_Shelterwood',
    // schedule: { signal : 'Auszug_completed' },
    schedule: {
        min: Math.round(106/SpeedFactor), 
        opt: Math.round(110/SpeedFactor), 
        max: Math.round(200/SpeedFactor), 
        force: true 
    },
    nTrees: Math.round(65*SpeedFactor), // = Z-Baumzahl in Df
    nCompetitors: Math.round(1000*SpeedFactor), // So hoch, um sicherzustellen, dass nur die Samenbäume verbleiben?
    interval: Math.round(3/SpeedFactor), 
    times: 3, // = 1 Besamungshieb, 2 Lichtungshiebe
    preferenceFunction: 'height / (stress * 0.8 + 1)', 
    speciesSelectivity: {
       // LB:
        "quro": 2.5, // sehr starke Bevorzugung
        "qupe": 2.5,
        "fasy": 0.3, // Buche stark reduziert, um Eichen zu fördern
        "acps": 1.0, 
        "acpl": 1.0,
        "acca": 1.0,
        "casa": 1.0,
        "bepe": 1.0,
        "soar": 1.0,
        "frex": 1.0,
        "cabe": 1.0,
        "tico": 1.0,
        "potr": 1.0,
        "quru": 1.3,
        "algl": 1.0,
        "ulgl": 1.0,
        "saca": 1.0,
        // NB:
        "psme": 0.3,
        "piab": 0.1,
        "pisy": 0.9,
        "pini": 0.9,
        "lade": 0.6,
        "abal": 0.3,
        // ohne Ziel-BHD-Angabe in WET2024:
        "qupu": 1.3,
        "soau": 1.0,
        "coav": 1.0
    },
    finalHarvest: true,
});

// Pflanzung - Umbau
const kx_SelectPatches = lib.selectOptimalPatches({
    id: 'kx_SelectPatches',
    schedule: {
        min: Math.round(1/SpeedFactor), 
        opt: Math.round(1/SpeedFactor), 
        max: Math.round(1/SpeedFactor), 
        force: true 
    },
    N: Math.round(2*SpeedFactor),
    patchId: 1,
    patchsize: 5, 
    spacing: 2, 
    criterium: 'max_basalarea_absolute',
    sendSignal: 'PatchesSelected'
});

const kx_ClearSaplings = { type: 'general',
    schedule: { signal: 'PatchesSelected' },
    action: function(){
        stand.simulate=false;
        // stand.trees.simulate=false;
        stand.trees.loadAll();
        var removedSaplings = stand.trees.killSaplings('height>0 and patch=1');
        fmengine.log("Killed " + removedSaplings + " saplings");
        // stand.trees.filter('species=fasy and dbh>60 or (species=bepe or species=potr) and dbh>40 or (species=piab or species=psme or species=lade or species=pisy or species=abal) and dbh>50');
        // fmengine.log("Target diameter cut, harvesting #" + stand.trees.harvest() + " trees, at stand topheight: " + stand.topHeight);
        stand.stp.signal('Saplings_cleared');
    }
};

const kx_Pflanzung = lib.planting.dynamic({
    id: 'kx_Pflanzung',
    schedule: { signal: 'Saplings_cleared' },
    // schedule: {
    //     min: Math.round(97/SpeedFactor), 
    //     opt: Math.round(97/SpeedFactor), 
    //     max: Math.round(97/SpeedFactor), 
    //     force: true 
    // }, // jetzt sollte beim 1. Femeln gepflanzt werden (Alter 95 = init Femel; Alter 96 = 1. Femel laut log)
    // //schedule: { signal: 'Pflanzung_Lichtbaumarten' },
    patches: 1,
    speciesSelectivity: function() {return dynamicPlantingSelectivity()},
    speciesDefaults: lib.planting.speciesDefaults, 
});

// kx_Tending, kx_Thinning, kx_Vorbereitungshieb, kx_Shelterwood, kx_SelectPatches, kx_ClearSaplings, kx_Pflanzung
lib.createSTP('WET_kx', kx_Tending, kx_Thinning, kx_Vorbereitungshieb, kx_Shelterwood, kx_SelectPatches, kx_ClearSaplings, kx_Pflanzung);