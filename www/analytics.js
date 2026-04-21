window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', 'G-C755LFBN2T');  // Replace with your GA tracking ID

// Track page visits
gtag('event', 'page_view', {
  'event_category': 'Site Engagement',
  'event_label': 'Wastewater Dashboard'
});

// Track clicks on "Respiratory Virus Data" menu item
$(document).on('click', 'a[data-value="overview"]', function() {
  gtag('event', 'menu_click', {
    'event_action': 'Click',
    'event_category': 'Navigation',
    'event_label': 'Respiratory Virus Data'
  });
});

// Track clicks on "About the Dashboard" menu item
$(document).on('click', 'a[data-value="technical_notes"]', function() {
  gtag('event', 'menu_click', {
    'event_action': 'Click',
    'event_category': 'Navigation',
    'event_label': 'About the Dashboard'
  });
});

// Track clicks on "Instructions" menu item
$(document).on('click', 'a[data-value="instructions"]', function() {
  gtag('event', 'menu_click', {
    'event_action': 'Click',
    'event_category': 'Navigation',
    'event_label': 'Instructions'
  });
});

// Track clicks on "Data Download" menu item
$(document).on('click', 'a[data-value="download"]', function() {
  gtag('event', 'menu_click', {
    'event_action': 'Click',
    'event_category': 'Navigation',
    'event_label': 'Data Download'
  });
});

// analytics.js

// Track clicks on "COVID" info box
$(document).on('click', '#home_covid', function() {
  gtag('event', 'info_box_click', {
    'event_action': 'Click',
    'event_category': 'Info Box',
    'event_label': 'COVID'
  });
});

// Track clicks on "Flu A" info box
$(document).on('click', '#home_fluA', function() {
  gtag('event', 'info_box_click', {
    'event_action': 'Click',
    'event_category': 'Info Box',
    'event_label': 'Flu A'
  });
});

// Track clicks on "Flu B" info box
$(document).on('click', '#home_fluB', function() {
  gtag('event', 'info_box_click', {
    'event_action': 'Click',
    'event_category': 'Info Box',
    'event_label': 'Flu B'
  });
});

// Track clicks on "RSV" info box
$(document).on('click', '#home_rsv', function() {
  gtag('event', 'info_box_click', {
    'event_action': 'Click',
    'event_category': 'Info Box',
    'event_label': 'RSV'
  });
});

// Track tab changes in the "Overview" tabsetPanel
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'tab') {
    gtag('event', 'tab_switch', {
      'event_category': 'Overview Tabs',
      'event_action': 'Switch',
      'event_label': event.value  // e.g., "Statewide", "Region", etc.
    });
  }
});

// Track changes to the "pathogen" selectInput
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'pathogen') {
    gtag('event', 'pathogen_select', {
      'event_category': 'Dropdown',
      'event_action': 'Select',
      'event_label': event.value  // e.g., "Flu A", "RSV", etc.
    });
  }
});

// Track changes to "flu_switch_A"
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'flu_switch_A') {
    gtag('event', 'switch_toggle', {
      'event_category': 'Switch Input',
      'event_action': 'Toggle',
      'event_label': 'Flu A Switch',
      'value': event.value ? 1 : 0
    });
  }
});

// Track changes to "h5_switch"
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'h5_switch') {
    gtag('event', 'switch_toggle', {
      'event_category': 'Switch Input',
      'event_action': 'Toggle',
      'event_label': 'Flu A (H5) Switch',
      'value': event.value ? 1 : 0
    });
  }
});

// Track changes to "flu_switch_B"
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'flu_switch_B') {
    gtag('event', 'switch_toggle', {
      'event_category': 'Switch Input',
      'event_action': 'Toggle',
      'event_label': 'Flu B Switch',
      'value': event.value ? 1 : 0
    });
  }
});

let regionViewStartTime = null;
let currentRegionView = null;

$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'region_toggle') {
    const now = new Date();

    // If switching away from a previous view, send duration
    if (regionViewStartTime && currentRegionView) {
      const durationSeconds = Math.round((now - regionViewStartTime) / 1000);
      gtag('event', 'region_view_duration', {
        'event_category': 'Region View',
        'event_action': 'Time Spent',
        'event_label': currentRegionView,
        'value': durationSeconds
      });
    }

    // Start timing the new view
    currentRegionView = event.value;  // "Overview" or "Each Region"
    regionViewStartTime = now;
  }
});

// Optional: send final duration when user leaves the page
window.addEventListener('beforeunload', function () {
  if (regionViewStartTime && currentRegionView) {
    const durationSeconds = Math.round((new Date() - regionViewStartTime) / 1000);
    gtag('event', 'region_view_duration', {
      'event_category': 'Region View',
      'event_action': 'Time Spent',
      'event_label': currentRegionView,
      'value': durationSeconds
    });
  }
});

// Track clicks on the Leaflet map with outputId "heatmap_region"
$(document).on('click', '#heatmap_region', function() {
  gtag('event', 'map_click', {
    'event_category': 'Leaflet Map',
    'event_action': 'Click',
    'event_label': 'Each Region Heatmap'
  });
});

// Track clicks on the Leaflet map with outputId "heatmap_sewershed"
$(document).on('click', '#heatmap_sewershed', function() {
  gtag('event', 'map_click', {
    'event_category': 'Leaflet Map',
    'event_action': 'Click',
    'event_label': 'Each Region Heatmap'
  });
});

// Track clicks on the "Download Data" button
$(document).on('click', '#confirmDownload', function() {
  gtag('event', 'download_click', {
    'event_category': 'Button',
    'event_action': 'Click',
    'event_label': 'Confirm Download'
  });
});


// Track clicks on the "Download Data" downloadButton
$(document).on('click', '#downloadData2', function() {
  gtag('event', 'download_click', {
    'event_category': 'Download',
    'event_action': 'Click',
    'event_label': 'Download Data 2'
  });
});

